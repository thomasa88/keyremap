use std::{
    collections::{btree_map, BTreeMap, btree_map::Entry::Occupied},
    thread::sleep,
    time::Duration,
};

use anyhow::{bail, Context, Result};
use evdev::{uinput::VirtualDeviceBuilder, EventType, InputEvent, InputEventKind, Key};
use num_traits::FromPrimitive;
use scopeguard::guard;
use tracing::info;

#[derive(num_derive::FromPrimitive, Debug, PartialEq)]
enum KeyAction {
    Release = 0,
    Press = 1,
    Repeat = 2,
}

#[derive(Clone, Copy, PartialEq)]
enum KeyState {
    Released,
    Pressed,
}

#[derive()]
struct ChordState {
    key_states: BTreeMap<Key, KeyState>,
    active_until: Option<std::time::Instant>,
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
    // (&mut chords.first().unwrap().keys).first_entry();
    // let mut m = BTreeMap::new();
    // m.insert(1, 2);
    // m.entry(1).and_modify(|n| *n = 2);
    // let mut v = vec![&mut space_chord];
    // v[0].keys.entry(Key::BTN_0).and_modify(|n| *n = KeyState::Pressed);
    // chords.get(0).unwrap().keys.entry(Key::BTN_0);
    #[derive(PartialEq, Debug)]
    enum S {
        None,
        First,
        Second,
    }
    let mut state = S::None;
    loop {
        for ev in dev.fetch_events()? {
            if let InputEventKind::Key(keycode) = ev.kind() {
                let action: KeyAction = FromPrimitive::from_i32(ev.value())
                    .context("Failed to get value from {ev:?}")?;

                let new_state_opt = match action {
                    KeyAction::Press => Some(KeyState::Pressed),
                    KeyAction::Release => Some(KeyState::Released),
                    KeyAction::Repeat => None,
                };
                if let Some(new_state) = new_state_opt {
                    for chord in &mut chords {
                        let mut state_changed = false;

                        let Occupied(entry) = chord.key_states.entry(keycode) else { continue; };

                        if chord.flushing

                        // chord.keys.entry(code).and_modify(|e| *e = s);
                        if let btree_map::Entry::Occupied(mut k) = chord.key_states.entry(keycode) {
                            *k.get_mut() = new_state;
                            if new_state == KeyState::Pressed && !chord.flushing {
                                chord.num_pressed += 1;
                            } else {
                                chord.num_pressed -= 1;
                            }

                            state_changed = true;

                            // uppfyllt (alla sanna)
                            // annars   .. borde bara kunna vara inaktiv om man går released -> pressed
                        }

                        if state_changed {
                            let mut send_events: Vec<InputEvent> = Vec::new();

                            // If the user releases a key, there can't be a fulfilled chord.
                            // However, we can't just send all the releases at once, as the user is still holding down
                            // some keys.
                            if action == KeyAction::Release {
                                chord.flushing = true;
                                if chord.key_states
                                send_events.push(key_event(keycode, KeyAction::Release));

                                // Release all pressed keys
                                // for (k, s) in &mut chord.key_states.iter_mut() {
                                //     // TODO: Keep order?
                                //     if *s == KeyState::Pressed {
                                //         // User tried to push the key
                                //         send_events.push(key_event(*k, KeyAction::Press));
                                //         send_events.push(key_event(*k, KeyAction::Release));
                                //     }
                                //     *s = KeyState::Released;
                                if chord.num_pressed == 0 {
                                    chord.flushing = false;
                                }
                            } else if action == KeyAction::Press {
                                if !chord.flushing {
                                    // Is the chord fully built?
                                    if chord.key_states.values().all(|&s| s == KeyState::Pressed) {
                                        // Chord fulfilled!
                                        // send_events.extend((chord.execute)().into_iter());
                                        send_events.extend((chord.execute)());

                                        // Swallowing the events. Swallow until all of the chord keys have been released again.
                                        chord.flushing = true;
                                    }
                                }
                            // } else if timeout
                            } else {
                                bail!("Bad action state: {action:?}");
                            }

                            if !send_events.is_empty() {
                                virt_kb.emit(&send_events)?;
                            }
                        }
                    }
                }
                // let mut matched = false;
                // let action = ev.value();
                // if code == Key::KEY_J {
                //     state = match state {
                //         S::None if action == KeyAction::Press as i32 => S::First,
                //         S::First | S::Second if action == KeyAction::Release as i32 => S::None,
                //         _ => state,
                //     };
                //     matched = true;
                // } else if code == Key::KEY_K {
                //     state = match state {
                //         S::First if action == KeyAction::Press as i32 => S::Second,
                //         S::First | S::Second if action == KeyAction::Release as i32 => S::None,
                //         _ => state,
                //     };
                //     matched = true;
                // }
                // info!(?state);
                // if state == S::Second {
                //     let down = InputEvent::new(
                //         EventType::KEY,
                //         Key::KEY_SPACE.code(),
                //         KeyAction::Press as i32,
                //     );
                //     let up = InputEvent::new(
                //         EventType::KEY,
                //         Key::KEY_SPACE.code(),
                //         KeyAction::Release as i32,
                //     );
                //     virt_kb.emit(&[down, up])?;
                //     state = S::None;
                // }
                // info!(?state);
                // if matched {
                //     // Event processed
                //     continue;
                // }
            }

            // Just forward all unprocessed events
            virt_kb.emit(&[ev])?;
        }
    }
}

fn key_event(key: Key, action: KeyAction) -> InputEvent {
    InputEvent::new(EventType::KEY, key.code(), action as i32)
}
