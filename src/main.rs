use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::BTreeMap,
    fmt,
    sync::BarrierWaitResult,
    thread::sleep,
    time::Duration,
    vec,
};

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

#[derive(Clone, Copy, PartialEq, Debug)]
enum KeyState {
    Released,
    Pressed,
}

// #[derive(Debug)]
struct ChordState<'a> {
    key_states: BTreeMap<Key, KeyState>,
    active_until: Option<std::time::SystemTime>,
    // Using an Fn instead of just a vec of InputEvent, to be able to store e.g. layer state
    execute: &'a mut dyn FnMut() -> Option<Vec<InputEvent>>,
    num_pressed: usize,
    exiting: bool,
    silent_exit: bool,
}

impl<'a> ChordState<'a> {
    fn new(keys: &[Key], callback: &'a mut dyn FnMut() -> Option<Vec<InputEvent>>) -> Self {
        Self {
            key_states: keys
                .iter()
                .map(|key| (key.clone(), KeyState::Released))
                .collect(),
            active_until: None,
            execute: callback,
            num_pressed: 0,
            exiting: false,
            silent_exit: false,
        }
    }
}

// struct ExecuteFn<'a> (&'a dyn Fn() -> Option<Vec<InputEvent>>);

// impl<'a> fmt::Debug for ExecuteFn<'a> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         f.write_str("ExecuteFn")
//     }
// }

// Manual impl of Debug, since dyn Fn() is not Debug
// TODO: How to make execute Debug instead?
impl<'a> fmt::Debug for ChordState<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ChordState")
            .field("key_states", &self.key_states)
            .field("active_until", &self.active_until)
            .field("num_pressed", &self.num_pressed)
            .field("aborting", &self.exiting)
            .field("releasing", &self.silent_exit)
            .finish_non_exhaustive()
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
    println!("Please release any key presses");
    sleep(Duration::from_millis(1000));
    dev.grab()?;
    println!("Keyboard grabbed!");

    let action = &mut || {
        Some(vec![
            key_event(Key::KEY_SPACE, KeyAction::Press),
            key_event(Key::KEY_SPACE, KeyAction::Release),
        ])
    };
    let mut space_chord = ChordState::new(&[Key::KEY_J, Key::KEY_K], action);
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
            proc.process(ev)?;
        }
    }
}

fn key_event(key: Key, action: KeyAction) -> InputEvent {
    InputEvent::new(EventType::KEY, key.code(), action as i32)
}

struct Processor<'a, T>
where
    T: Fn(&[InputEvent]),
{
    chords: &'a mut [&'a mut ChordState<'a>],
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
    fn new(chords: &'a mut [&'a mut ChordState<'a>], emit: T) -> Self {
        Self {
            chords,
            _emit: emit,
        }
    }

    fn process(&mut self, ev: InputEvent) -> Result<()> {
        if ev.event_type() == EventType::KEY {
            match self.process_key_event(ev) {
                Ok(ProcessResult::Processed) => (),
                // Pass keys through if not process or if their is a failure
                Ok(ProcessResult::NotProcessed) => (self._emit)(&[ev]),
                Err(e) => {
                    (self._emit)(&[ev]);
                    bail!("{}\n{:?}", e, self.chords)
                }
            };
        } else {
            (self._emit)(&[ev]);
        }
        Ok(())
    }

    fn process_key_event(&mut self, ev: InputEvent) -> Result<ProcessResult> {
        let InputEventKind::Key(event_key) = ev.kind() else {
            panic!("Bad event kind {:?}", ev.kind());
        };

        let mut result = ProcessResult::NotProcessed;
        for chord in self.chords.iter_mut() {
            // if let Occupied(mut key_state) = chord.key_states.entry(event_key) {
            if chord.key_states.contains_key(&event_key) {
                let mut chord_result = ProcessResult::Processed;
                // Key in chord
                let action = FromPrimitive::from_i32(ev.value())
                    .context(format!("Bad action: {}", ev.value()))?;
                let new_state_opt = match action {
                    KeyAction::Press => Some(KeyState::Pressed),
                    KeyAction::Release => Some(KeyState::Released),
                    KeyAction::Repeat => None,
                };
                if action == KeyAction::Repeat
                    && chord.num_pressed == 1
                    // Should not really need to check that the key is pressed,
                    // as repeat implies pressed
                    && *chord.key_states.get(&event_key).unwrap() == KeyState::Pressed
                {
                    if chord.exiting {
                        chord_result = ProcessResult::NotProcessed;
                    } else {
                        // Allow a normal repeat of a key if it is the first of the chord
                        // The chord will be broken
                        Self::send_captured_keys(&self._emit, chord, ev)?;
                        chord.exiting = true;
                    }
                } else if let Some(new_state) = new_state_opt {
                    if chord.exiting {
                        assert_ne!(chord.num_pressed, 0);
                        // When releasing a chord, we should still silent the key events
                        let silence = chord.silent_exit;
                        if new_state == KeyState::Released {
                            chord.num_pressed -= 1;
                            if chord.num_pressed == 0 {
                                chord.exiting = false;
                                chord.silent_exit = false;
                            }
                        } else if new_state == KeyState::Pressed {
                            // Must count this even when releasing the chord,
                            // as we do num_pressed -=1 in the other branch.
                            // Alternative: Ignore pressed event and only decrement
                            // if the old state is released.
                            chord.num_pressed += 1;
                        }
                        if !silence {
                            // Pass the event through
                            chord_result = ProcessResult::NotProcessed;
                        }
                    } else if new_state == KeyState::Pressed {
                        chord.num_pressed += 1;

                        if chord.num_pressed == chord.key_states.len() {
                            // Chord completed!
                            let chord_result = (chord.execute)();
                            if let Some(chord_seq) = chord_result {
                                (self._emit)(&chord_seq);
                            }
                            chord.silent_exit = true;
                            chord.exiting = true;
                        }
                    } else if new_state == KeyState::Released {
                        // Chord is broken
                        if chord.num_pressed > 0 {
                            chord.num_pressed -= 1;

                            Self::send_captured_keys(&self._emit, chord, ev)?;

                            if chord.num_pressed > 0 {
                                // The flushing was not completed in one step - let it continue
                                chord.exiting = true;
                            }
                        } else {
                            bail!("Tried to decrement number of pressed keys below 0! Key event: {ev:?}")
                        }
                    };
                    // Update with the latest state after any flushing, to not miss the key while flushing
                    // That is, if the key was pressed, the press event should be flushed befor we set the
                    // state to released.
                    *chord.key_states.get_mut(&event_key).unwrap() = new_state;
                }
                result = chord_result;
            } else {
                // Key not in chord

                if !chord.exiting && chord.num_pressed > 0 {
                    // Key outside of active chord. Chord is broken!
                    Self::send_captured_keys(&self._emit, chord, ev)?;
                    chord.exiting = true;
                    result = ProcessResult::Processed;
                }
            }
        }

        Ok(result)
    }

    // Passing emit instead of self, as passing self results in a borrow on the whole struct instance
    // (which ends up a nested borrow in process_key_event)
    fn send_captured_keys(emit: &T, chord: &mut ChordState, ev: InputEvent) -> Result<()> {
        let mut send_events = vec![];
        // Propagate the keys that are pressed, but were grabbed when building the chord
        for (key, key_state) in chord.key_states.iter() {
            if *key_state == KeyState::Pressed {
                send_events.push(key_event(*key, KeyAction::Press));
            }
        }
        send_events.push(ev);
        (emit)(&send_events);
        Ok(())
    }

    // fn emit(&self, events: &[InputEvent]) {
    //     (self._emit)(&events);
    // }
}

fn event_str(event: &InputEvent) -> String {
    format!(
        "InputEvent {{ kind: {:?}, action: {:?}, ...}}",
        event.kind(),
        <i32 as TryInto<KeyAction>>::try_into(event.value())
    )
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, iter::zip};

    use super::*;
    use KeyAction::*;

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
            proc.process(*e)?;
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
    }

    impl Catcher {
        fn new() -> Self {
            Self {
                events: RefCell::new(Vec::new()),
                // c: Box::new(|_| {}),
            }
        }

        fn catch_emit(&self) -> impl Fn(&[InputEvent]) + '_ {
            |ev: &[InputEvent]| {
                println!("emit({:?})", &ev);
                self.events.borrow_mut().extend(ev);
            }
        }

        // TODO: Should be &mut self
        fn reset(&self) {
            self.events.borrow_mut().clear();
        }
    }

    #[test]
    fn test_free_key() -> Result<()> {
        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut [], catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_A, Repeat),
            (Key::KEY_A, Release),
        ]);
        input_keys(&mut proc, &keys)?;

        assert_events_eq(&catcher.events.borrow(), &keys);
        Ok(())
    }

    #[test]
    fn test_chord_first_key_press() -> Result<()> {
        let mut binding = || panic!("Chord executed");
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[(Key::KEY_A, Press)]);
        input_keys(&mut proc, &keys)?;

        assert_events_eq(&catcher.events.borrow(), &[]);
        Ok(())
    }

    #[test]
    fn test_chord_second_key_press() -> Result<()> {
        let mut binding = || panic!("Chord executed");
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[(Key::KEY_A, Press), (Key::KEY_B, Press)]);
        input_keys(&mut proc, &keys)?;

        assert_events_eq(&catcher.events.borrow(), &[]);
        Ok(())
    }

    #[test]
    fn test_chord_broken_by_other_key() -> Result<()> {
        // All keys should flush if the chord is broken

        let mut binding = || panic!("Chord executed");
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_G, Release),
        ]);
        input_keys(&mut proc, &keys)?;

        assert_events_eq(&catcher.events.borrow(), &keys);
        Ok(())
    }

    #[test]
    fn test_chord_broken_by_release() -> Result<()> {
        // All keys should flush if the chord is broken

        let mut binding = || panic!("Chord executed");
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_A, Release),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &keys);
        Ok(())
    }

    #[test]
    fn test_passthrough_while_flushing() -> Result<()> {
        // All events should be passed through when a chord is breaking up

        let mut binding = || panic!("Chord executed");
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            // Break the chord before completing it
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

    #[test]
    fn test_complete_chord() -> Result<()> {
        let catcher = Catcher::new();
        const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

        let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_C, Press),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

        // User releases the chord press
        let keys = key_ev_seq(&[
            (Key::KEY_A, Release),
            (Key::KEY_C, Release),
            (Key::KEY_B, Release),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

        Ok(())
    }

    #[test]
    fn test_passthrough_after_chord() -> Result<()> {
        let catcher = Catcher::new();
        const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

        let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            // Press chord
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_C, Press),
            // We allow new presses before the chord is fully release
            (Key::KEY_G, Press),
            // Release chord
            (Key::KEY_A, Release),
            (Key::KEY_B, Release),
            (Key::KEY_C, Release),
            // Key after release
            (Key::KEY_G, Release),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(
            &catcher.events.borrow(),
            &key_ev_seq(&[CHORD_OUTPUT, &[(Key::KEY_G, Press), (Key::KEY_G, Release)]].concat()),
        );

        Ok(())
    }

    #[test]
    fn test_chord_after_chord() -> Result<()> {
        let catcher = Catcher::new();
        const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

        let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_C, Press),
            (Key::KEY_A, Release),
            (Key::KEY_B, Release),
            (Key::KEY_C, Release),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

        // Any input order is OK
        catcher.reset();
        let keys = key_ev_seq(&[
            (Key::KEY_B, Press),
            (Key::KEY_C, Press),
            (Key::KEY_A, Press),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

        Ok(())
    }

    #[test]
    fn test_chord_after_flush() -> Result<()> {
        let mut executed = false;
        let mut binding = || {
            executed = true;
            None
        };
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            // Break the chord - resulting in a flush
            (Key::KEY_G, Press),
            // Release the chord
            (Key::KEY_B, Release),
            (Key::KEY_A, Release),
            // Input the full chord
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_C, Press),
        ]);
        let output_keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_G, Press),
            (Key::KEY_B, Release),
            (Key::KEY_A, Release),
        ]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &output_keys);
        assert!(executed);
        Ok(())
    }

    #[test]
    fn test_chord_after_chord_key() -> Result<()> {
        // Test for a bug

        let mut executed = false;
        let mut binding = || {
            executed = true;
            None
        };
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            (Key::KEY_A, Press),
            (Key::KEY_A, Release),
            // Input Chord
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_C, Press),
            // Release the chord
            (Key::KEY_A, Release),
            (Key::KEY_B, Release),
            (Key::KEY_C, Press),
        ]);
        let output_keys = key_ev_seq(&[(Key::KEY_A, Press), (Key::KEY_A, Release)]);
        input_keys(&mut proc, &keys)?;
        dbg!(&proc.chords);
        assert_events_eq(&catcher.events.borrow(), &output_keys);
        assert!(executed);
        Ok(())
    }

    #[test]
    fn test_chord_key_pressed_again_while_aborting() -> Result<()> {
        // Test for a bug

        let mut binding = || None;
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            // Chord
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_C, Press),
            // Make sure the total number of releases is more than the
            // number of keys in the chord
            (Key::KEY_A, Release),
            (Key::KEY_B, Release),
            (Key::KEY_A, Press),
            (Key::KEY_C, Release),
            (Key::KEY_A, Release),
        ]);
        let output_keys = key_ev_seq(&[]);
        input_keys(&mut proc, &keys)?;
        dbg!(&proc.chords);
        assert_events_eq(&catcher.events.borrow(), &output_keys);
        Ok(())
    }

    #[test]
    fn test_key_repeats_in_chord() -> Result<()> {
        let mut executed = false;
        let mut binding = || {
            executed = true;
            None
        };
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            // Input the full chord
            (Key::KEY_A, Press),
            (Key::KEY_B, Press),
            (Key::KEY_A, Repeat),
            (Key::KEY_A, Repeat),
            (Key::KEY_B, Repeat),
            (Key::KEY_C, Press),
            // And release
            (Key::KEY_A, Release),
            (Key::KEY_B, Release),
            (Key::KEY_C, Release),
        ]);
        let output_keys = key_ev_seq(&[]);
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &output_keys);
        assert!(executed);
        Ok(())
    }

    #[test]
    fn test_repeat_chord_key_outside_chord() -> Result<()> {
        // User should be able to repeat a chord key if it is not building up a chord

        let mut binding = || None;
        let mut chords = [&mut ChordState::new(
            &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
            &mut binding,
        )];

        let catcher = Catcher::new();
        let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        let keys = key_ev_seq(&[
            // Press a key that is part of the chord
            (Key::KEY_A, Press),
            (Key::KEY_A, Repeat),
            (Key::KEY_A, Repeat),
            (Key::KEY_A, Repeat),
            (Key::KEY_A, Repeat),
            (Key::KEY_A, Release),
        ]);
        let output_keys = &keys;
        input_keys(&mut proc, &keys)?;
        assert_events_eq(&catcher.events.borrow(), &output_keys);
        Ok(())
    }

    // #[test]
    // fn test_chord_timeout() -> Result<()> {
    //     // Keys must be passed through if the user did not intend
    //     // to use them in a chord.

    //     let mut action = || None;
    //     let mut chords = [&mut ChordState::new(
    //         &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //         &mut action,
    //     )];

    //     let catcher = Catcher::new();
    //     let mut proc = Processor::new(&mut chords, catcher.catch());

    //     let keys = key_ev_seq(&[
    //         // Input partial chord
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         // Time out
    //     ]);
    //     let output_keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     Ok(())
    // }

    // fn test_flush_in_input_order()
}
