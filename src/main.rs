use std::{cell::RefCell, collections::BTreeMap, thread::sleep, time::Duration, vec};

use anyhow::{bail, Context, Result};
use evdev::{uinput::VirtualDeviceBuilder, EventType, InputEvent, InputEventKind, Key};
use itertools::Itertools;
use num_traits::FromPrimitive;
use scopeguard::guard;
use tracing::info;

#[derive(num_derive::FromPrimitive, Debug, PartialEq, Clone, Copy)]
enum KeyAction {
    Release = 0,
    Press = 1,
    Repeat = 2,
}

impl TryFrom<i32> for KeyAction {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> std::prelude::v1::Result<Self, Self::Error> {
        FromPrimitive::from_i32(value).context(format!("Bad key action value: {value}"))
    }
}

#[derive(Clone, Copy, PartialEq)]
enum KeyState {
    Released,
    Pressed,
}

#[derive()]
struct ChordState {
    key_states: BTreeMap<Key, KeyState>,
    active_until: Option<std::time::SystemTime>,
    execute: fn() -> Vec<InputEvent>,
    num_pressed: usize,
    flushing: bool,
}

impl ChordState {
    fn new(keys: &[Key], callback: fn() -> Vec<InputEvent>) -> Self {
        Self {
            key_states: keys
                .iter()
                .map(|key| (key.clone(), KeyState::Released))
                .collect(),
            active_until: None,
            execute: callback,
            num_pressed: 0,
            flushing: false,
        }
    }
}

fn main() -> Result<()> {
    tracing_subscriber::fmt().init();

    let mut unguarded_dev =
        evdev::Device::open("/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-kbd")?;
    let mut dev = guard(&mut unguarded_dev, |d| {
        d.ungrab().unwrap();

        // Releasing keys on the virt_kb is likely not needed,
        // as the device is removed when exiting.
    });

    let keys = dev.supported_keys().context("Failed to get keys")?;

    let mut virt_kb = VirtualDeviceBuilder::new()?
        .name("keyremap")
        .with_keys(&keys)?
        .build()?;

    for path in virt_kb.enumerate_dev_nodes_blocking()? {
        println!("Virtual keyboard path: {}", path?.display());
    }

    // Wait for any pressed key (used to start the command) to be released before grabbing...
    // TODO: Make sure to release all keys.. (loop all of them? :O) Is it possible to check which keys are down?
    // ioctl(EVIOCGKEY) can read the current state of a key.
    sleep(Duration::from_millis(1000));
    dev.grab()?;

    let mut space_chord = ChordState::new(&[Key::KEY_J, Key::KEY_K], || {
        vec![
            key_event(Key::KEY_SPACE, KeyAction::Press),
            key_event(Key::KEY_SPACE, KeyAction::Release),
        ]
    });
    let mut chords = [&mut space_chord];
    // Wrapping the keyboard to be able to set up an emit() that is not mutable
    let vkb = RefCell::new(virt_kb);
    // let emit_wrapper = RefCell::new(|m| {
    //     &virt_kb.emit(m).ok();
    // });
    // let emit = |e: &[InputEvent]| (emit_wrapper.borrow_mut())(e);
    let emit = |e: &[InputEvent]| {
        vkb.borrow_mut().emit(e).ok();
    };
    let mut proc = Processor::new(&mut chords, emit);
    loop {
        for ev in dev.fetch_events()? {
            process(proc, ev);
        }
    }
}

fn process(mut proc: Processor<impl Fn(&[InputEvent])>, ev: InputEvent) -> Result<()> {
    if ev.event_type() == EventType::KEY {
        match proc.process_key_event(ev) {
            Ok(ProcessResult::Processed) => (),
            // Pass keys through if not process or if their is a failure
            Ok(ProcessResult::NotProcessed) => (proc._emit)(&[ev]),
            Err(e) => {
                (proc._emit)(&[ev]);
                bail!(e)
            }
        };
    } else {
        (proc._emit)(&[ev]);
    }
    Ok(())
}

fn key_event(key: Key, action: KeyAction) -> InputEvent {
    InputEvent::new(EventType::KEY, key.code(), action as i32)
}

struct Processor<'a, T>
where
    T: Fn(&[InputEvent]),
{
    chords: &'a mut [&'a mut ChordState],
    _emit: T,
}

#[derive(Debug, PartialEq)]
enum ProcessResult {
    Processed,
    NotProcessed,
}

impl<'a, T> Processor<'a, T>
where
    T: Fn(&[InputEvent]),
{
    fn new(chords: &'a mut [&'a mut ChordState], emit: T) -> Self {
        Self {
            chords,
            _emit: emit,
        }
    }

    fn process_key_event(&mut self, ev: InputEvent) -> Result<ProcessResult> {
        // assert_eq!(ev.event_type(), EventType::KEY);
        let InputEventKind::Key(event_key) = ev.kind() else {
            panic!("Bad key kind");
        };

        for chord in self.chords.iter_mut() {
            // if let Occupied(mut key_state) = chord.key_states.entry(event_key) {
            if chord.key_states.contains_key(&event_key) {
                // Key in chord
                let action = FromPrimitive::from_i32(ev.value())
                    .context(format!("Bad action: {}", ev.value()))?;
                let new_state_opt = match action {
                    KeyAction::Press => Some(KeyState::Pressed),
                    KeyAction::Release => Some(KeyState::Released),
                    KeyAction::Repeat => None,
                };
                if let Some(new_state) = new_state_opt {
                    if chord.flushing {
                        if new_state == KeyState::Released {
                            chord.num_pressed -= 1;
                            if chord.num_pressed == 0 {
                                chord.flushing = false;
                            }
                        }
                        // Pass the event through
                        dbg!(event_key, "flush");
                        return Ok(ProcessResult::NotProcessed);
                    } else if new_state == KeyState::Pressed {
                        chord.num_pressed += 1;
                    } else if new_state == KeyState::Released {
                        // Chord is broken
                        if chord.num_pressed > 0 {
                            chord.num_pressed -= 1;
                            Self::start_flush(&self._emit, chord, ev)?;
                        } else {
                            bail!("Tried to decrement number of pressed keys below 0!")
                        }
                    };
                    // Update with the latest state after any flushing
                    *chord.key_states.get_mut(&event_key).unwrap() = new_state;
                    //*key_state.get_mut() = new_state;
                }
                return Ok(ProcessResult::Processed);
            } else if chord.num_pressed > 0 {
                // Key outside of chord. Chord is broken. Flush!
                Self::start_flush(&self._emit, chord, ev)?;
                return Ok(ProcessResult::Processed);
            }
        }

        Ok(ProcessResult::NotProcessed)
    }

    // Passing emit instead of self, as passing self results in a borrow on the whole struct instance
    // (which ends up a nested borrow in process_key_event)
    fn start_flush(emit: &T, chord: &mut ChordState, ev: InputEvent) -> Result<()> {
        chord.flushing = true;
        let mut send_events = vec![];
        for (key, key_state) in chord.key_states.iter() {
            if *key_state == KeyState::Pressed {
                send_events.push(key_event(*key, KeyAction::Press));
            }
        }
        send_events.push(ev);
        //self.emit(&send_events);
        (emit)(&send_events);
        Ok(())
    }

    // fn emit(&self, events: &[InputEvent]) {
    //     (self._emit)(&events);
    // }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, iter::zip};

    use super::*;
    use KeyAction::*;

    fn event_str(event: &InputEvent) -> String {
        format!(
            "InputEvent {{ kind: {:?}, action: {:?}, ...}}",
            event.kind(),
            <i32 as TryInto<KeyAction>>::try_into(event.value())
        )
    }

    fn events_str(events: &[InputEvent]) -> String {
        ["[", &events.iter().map(|e| event_str(e)).join(",\n"), "]"].concat()
    }

    fn key_ev_seq(ev_infos: &[(Key, KeyAction)]) -> Vec<InputEvent> {
        ev_infos
            .into_iter()
            .map(|(k, a)| key_event(*k, *a))
            .collect()
    }

    fn input_keys(
        proc: &mut Processor<impl Fn(&[InputEvent])>,
        events: &[InputEvent],
    ) -> Result<()> {
        for e in events {
            proc.process_key_event(*e)?;
        }
        Ok(())
    }

    fn assert_event_eq(lhs: &InputEvent, rhs: &InputEvent) {
        assert_eq!(lhs.event_type(), rhs.event_type());
        assert_eq!(lhs.code(), rhs.code());
        assert_eq!(lhs.value(), rhs.value());
    }

    fn assert_events_eq(a: &[InputEvent], b: &[InputEvent]) {
        assert_eq!(
            a.len(),
            b.len(),
            "\n{}\n    !=\n{}",
            events_str(a),
            events_str(b)
        );
        for (lhs, rhs) in zip(a, b) {
            assert_event_eq(&lhs, &rhs);
        }
    }

    struct Catcher {
        events: RefCell<Vec<InputEvent>>,
        // c: Box<dyn Fn(&[InputEvent])>,
    }

    impl Catcher {
        // const c: OnceCell<Box<dyn Fn(&[InputEvent])>> = OnceCell::new();

        fn new() -> Self {
            Self {
                events: RefCell::new(Vec::new()),
                // c: Box::new(|_| {}),
            }
            // s.c = Box::new(|evs| {s.events.borrow_mut().extend(evs)});
            // s
        }

        fn catch(&self) -> impl Fn(&[InputEvent]) + '_ {
            // Self::c.get_or_init(move || {
            //     Box::new(|ev: &[InputEvent]| {
            //         self.events.borrow_mut().extend(ev);
            //     })
            // })
            |ev: &[InputEvent]| {
                self.events.borrow_mut().extend(ev);
            }
        }
        // fn catch(&self, events: &[InputEvent]) {
        //     self.events.borrow_mut().extend(events);
        // }
    }

    #[test]
    fn test_free_key() -> Result<()> {
        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut [], catcher.catch());

        let p = key_event(Key::KEY_A, Press);
        let r = key_event(Key::KEY_A, Release);

        assert_eq!(proc.process_key_event(p)?, ProcessResult::NotProcessed);
        assert_eq!(proc.process_key_event(r)?, ProcessResult::NotProcessed);

        assert_events_eq(&catcher.events.borrow(), &[]);
        Ok(())
    }

    #[test]
    fn test_chord_first_key_press() -> Result<()> {
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            || panic!("Chord executed"),
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch());

        let p = key_event(Key::KEY_A, Press);
        proc.process_key_event(p)?;

        assert_events_eq(&catcher.events.borrow(), &[]);
        Ok(())
    }

    #[test]
    fn test_chord_second_key_press() -> Result<()> {
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            || panic!("Chord executed"),
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch());

        let p = key_event(Key::KEY_A, Press);
        proc.process_key_event(p)?;
        let p = key_event(Key::KEY_B, Press);
        proc.process_key_event(p)?;

        assert_events_eq(&catcher.events.borrow(), &[]);
        Ok(())
    }

    #[test]
    fn test_chord_broken_by_other_key() -> Result<()> {
        // All keys should flush if the chord is broken

        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            || panic!("Chord executed"),
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch());

        let a = key_event(Key::KEY_A, Press);
        proc.process_key_event(a)?;
        let b = key_event(Key::KEY_B, Press);
        proc.process_key_event(b)?;
        let g = key_event(Key::KEY_G, Press);
        proc.process_key_event(g)?;

        assert_events_eq(&catcher.events.borrow(), &[a, b, g]);
        Ok(())
    }

    #[test]
    fn test_chord_broken_by_release() -> Result<()> {
        // All keys should flush if the chord is broken

        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            || panic!("Chord executed"),
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch());

        let a = key_event(Key::KEY_A, Press);
        proc.process_key_event(a)?;
        let b = key_event(Key::KEY_B, Press);
        proc.process_key_event(b)?;
        let ar = key_event(Key::KEY_A, Release);
        proc.process_key_event(ar)?;

        assert_events_eq(&catcher.events.borrow(), &[a, b, ar]);
        Ok(())
    }

    #[test]
    fn test_passthrough_while_flushing() -> Result<()> {
        // All events should be passed through when a chord is breaking up

        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            || panic!("Chord executed"),
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            // Break the chord
            (Key::KEY_G, Press),
            // And continue typing, even letters from the chord - as long as some parts of the chord is still pressed
            (Key::KEY_B, Release),
            (Key::KEY_B, Press),
            (Key::KEY_B, Release),
            (Key::KEY_I, Press),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &keys);
        Ok(())
    }

    // test_chord_after_flush

    // test_chord_after_chord

    // fn test_chord_timeout()
}
