use std::collections::BTreeMap;

use anyhow::{bail, Context, Result};
use evdev::{InputEvent, InputEventKind, Key};
use num_traits::FromPrimitive;

use crate::{ActionFn, HandleResult, KeyEventHandler, KeyEventValue, NiceKeyInputEvent, ProcView};

#[derive(Clone, Copy, PartialEq, Debug)]
enum KeyState {
    Released,
    Pressed,
}

// pub struct Chord<'a> {
//     keys: &'a [Key],
//     action: ActionFn,
// }

pub struct ChordHandler {
    // chord: Chord<'a>u
    action: ActionFn,
    key_states: BTreeMap<Key, KeyState>,
    active_until: Option<std::time::SystemTime>,
    num_pressed: usize,
    exiting: bool,
    silent_exit: bool,
}

impl ChordHandler {
    pub fn new(keys: &[Key], action: ActionFn) -> Self {
        Self {
            key_states: keys
                .iter()
                .map(|key| (key.clone(), KeyState::Released))
                .collect(),
            active_until: None,
            action: action,
            num_pressed: 0,
            exiting: false,
            silent_exit: false,
        }
    }

    fn send_captured_keys(pv: ProcView, chord: &mut ChordHandler) -> Result<()> {
        let mut send_events = vec![];
        // Propagate the keys that are pressed, but were grabbed when building the chord
        for (key, key_state) in chord.key_states.iter() {
            if *key_state == KeyState::Pressed {
                send_events.push(NiceKeyInputEvent::new(*key, KeyEventValue::Press).into());
            }
        }
        // TODO: process_event could return the event if unprocessed. Then it could be passed in as a move.
        let last_event: InputEvent = pv.event.clone().into();
        send_events.push(last_event);
        pv.output_kb.emit(&send_events)?;
        Ok(())
    }
}

impl KeyEventHandler for ChordHandler {
    fn handle_event(&mut self, pv: ProcView) -> Result<crate::HandleResult> {
        let mut result = HandleResult::NotHandled; // This only works because InputEvent is Clone. fix
        let event_key = pv.event.key;
        if self.key_states.contains_key(&event_key) {
            let mut chord_result = HandleResult::Handled;
            // Key in chord
            let new_state_opt = match pv.event.value {
                KeyEventValue::Press => Some(KeyState::Pressed),
                KeyEventValue::Release => Some(KeyState::Released),
                KeyEventValue::Repeat => None,
            };
            if pv.event.value == KeyEventValue::Repeat
                    && self.num_pressed == 1
                    // Should not really need to check that the key is pressed,
                    // as repeat implies pressed
                    && *self.key_states.get(&event_key).unwrap() == KeyState::Pressed
            {
                if self.exiting {
                    chord_result = HandleResult::NotHandled;
                } else {
                    // Allow a normal repeat of a key if it is the first of the chord
                    // The chord will be broken
                    ChordHandler::send_captured_keys(pv, self)?;
                    self.exiting = true;
                }
            } else if let Some(new_state) = new_state_opt {
                if self.exiting {
                    assert_ne!(self.num_pressed, 0);
                    // When releasing a chord, we should still silent the key events
                    let silence = self.silent_exit;
                    if new_state == KeyState::Released {
                        self.num_pressed -= 1;
                        if self.num_pressed == 0 {
                            self.exiting = false;
                            self.silent_exit = false;
                        }
                    } else if new_state == KeyState::Pressed {
                        // Must count this even when releasing the chord,
                        // as we do num_pressed -=1 in the other branch.
                        // Alternative: Ignore pressed event and only decrement
                        // if the old state is released.
                        self.num_pressed += 1;
                    }
                    if !silence {
                        // Pass the event through
                        chord_result = HandleResult::NotHandled;
                    }
                } else if new_state == KeyState::Pressed {
                    self.num_pressed += 1;

                    if self.num_pressed == self.key_states.len() {
                        // Chord completed!
                        chord_result = (self.action)(pv)?;
                        self.silent_exit = true;
                        self.exiting = true;
                    }
                } else if new_state == KeyState::Released {
                    // Chord is broken
                    if self.num_pressed > 0 {
                        self.num_pressed -= 1;

                        ChordHandler::send_captured_keys(pv, self)?;

                        if self.num_pressed > 0 {
                            // The flushing was not completed in one step - let it continue
                            self.exiting = true;
                        }
                    } else {
                        bail!(
                            "Tried to decrement number of pressed keys below 0! Key event: {:?}",
                            pv.event
                        )
                    }
                };
                // Update with the latest state after any flushing, to not miss the key while flushing
                // That is, if the key was pressed, the press event should be flushed befor we set the
                // state to released.
                *self.key_states.get_mut(&event_key).unwrap() = new_state;
            }
            result = chord_result;
        } else {
            // Key not in chord

            if !self.exiting && self.num_pressed > 0 {
                // Key outside of active chord. Chord is broken!
                ChordHandler::send_captured_keys(pv, self)?;
                self.exiting = true;
                result = HandleResult::Handled;
            }
        }
        Ok(result)
    }
    fn reset(&mut self) {
        // self.key_states.values_mut().for_each(|v| *v = )
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, iter::zip};

    use super::*;
    use crate::KeyEventValue::*;

    //     fn events_str(events: &[InputEvent]) -> String {
    //         ["[", &events.iter().map(|e| event_str(e)).join(",\n"), "]"].concat()
    //     }

    //     fn key_ev_seq(ev_infos: &[(Key, KeyAction)]) -> Vec<InputEvent> {
    //         ev_infos
    //             .into_iter()
    //             .map(|(k, a)| key_event(*k, *a))
    //             .collect()
    //     }

    //     fn input_keys(
    //         proc: &mut Processor<impl Fn(&[InputEvent])>,
    //         events: &[InputEvent],
    //     ) -> Result<()> {
    //         for e in events {
    //             proc.process(*e)?;
    //         }
    //         Ok(())
    //     }

    //     fn assert_event_eq(lhs: &InputEvent, rhs: &InputEvent) {
    //         assert_eq!(lhs.event_type(), rhs.event_type());
    //         assert_eq!(lhs.code(), rhs.code());
    //         assert_eq!(lhs.value(), rhs.value());
    //     }

    //     fn assert_events_eq(a: &[InputEvent], b: &[InputEvent]) {
    //         assert_eq!(
    //             a.len(),
    //             b.len(),
    //             "\n{}\n    !=\n{}",
    //             events_str(a),
    //             events_str(b)
    //         );
    //         for (lhs, rhs) in zip(a, b) {
    //             assert_event_eq(&lhs, &rhs);
    //         }
    //     }

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

    //     #[test]
    //     fn test_free_key() -> Result<()> {
    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut [], catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_A, Repeat),
    //             (Key::KEY_A, Release),
    //         ]);
    //         input_keys(&mut proc, &keys)?;

    //         assert_events_eq(&catcher.events.borrow(), &keys);
    //         Ok(())
    //     }

    #[test]
    fn test_chord_first_key_press() -> Result<()> {
        let mut binding = Box::new(|_| panic!("Chord executed"));
        let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], binding);

        let catcher = Catcher::new();
        let pv = ProcView {
            event: &NiceKeyInputEvent {
                key: Key::KEY_A,
                value: KeyEventValue::Press,
            },
            active_layer_id: &mut 0,
            output_kb: catcher,
        };
        assert_eq!(chord.handle_event(pv)?, HandleResult::Handled);


        // let mut proc = Processor::new(&mut chords, catcher.catch_emit());

        // let keys = key_ev_seq(&[(Key::KEY_A, Press)]);
        // input_keys(&mut proc, &keys)?;

        // assert_events_eq(&catcher.events.borrow(), &[]);
        Ok(())
    }

    //     #[test]
    //     fn test_chord_second_key_press() -> Result<()> {
    //         let mut binding = || panic!("Chord executed");
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[(Key::KEY_A, Press), (Key::KEY_B, Press)]);
    //         input_keys(&mut proc, &keys)?;

    //         assert_events_eq(&catcher.events.borrow(), &[]);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_chord_broken_by_other_key() -> Result<()> {
    //         // All keys should flush if the chord is broken

    //         let mut binding = || panic!("Chord executed");
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_G, Release),
    //         ]);
    //         input_keys(&mut proc, &keys)?;

    //         assert_events_eq(&catcher.events.borrow(), &keys);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_chord_broken_by_release() -> Result<()> {
    //         // All keys should flush if the chord is broken

    //         let mut binding = || panic!("Chord executed");
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_A, Release),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &keys);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_passthrough_while_flushing() -> Result<()> {
    //         // All events should be passed through when a chord is breaking up

    //         let mut binding = || panic!("Chord executed");
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             // Break the chord before completing it
    //             (Key::KEY_G, Press),
    //             // And continue typing, even letters from the chord - as long as some parts of the chord is still pressed
    //             (Key::KEY_B, Release),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_B, Release),
    //             (Key::KEY_I, Press),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &keys);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_complete_chord() -> Result<()> {
    //         let catcher = Catcher::new();
    //         const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

    //         let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_C, Press),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //         // User releases the chord press
    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Release),
    //             (Key::KEY_C, Release),
    //             (Key::KEY_B, Release),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //         Ok(())
    //     }

    //     #[test]
    //     fn test_passthrough_after_chord() -> Result<()> {
    //         let catcher = Catcher::new();
    //         const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

    //         let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             // Press chord
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_C, Press),
    //             // We allow new presses before the chord is fully release
    //             (Key::KEY_G, Press),
    //             // Release chord
    //             (Key::KEY_A, Release),
    //             (Key::KEY_B, Release),
    //             (Key::KEY_C, Release),
    //             // Key after release
    //             (Key::KEY_G, Release),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(
    //             &catcher.events.borrow(),
    //             &key_ev_seq(&[CHORD_OUTPUT, &[(Key::KEY_G, Press), (Key::KEY_G, Release)]].concat()),
    //         );

    //         Ok(())
    //     }

    //     #[test]
    //     fn test_chord_after_chord() -> Result<()> {
    //         let catcher = Catcher::new();
    //         const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

    //         let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_C, Press),
    //             (Key::KEY_A, Release),
    //             (Key::KEY_B, Release),
    //             (Key::KEY_C, Release),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //         // Any input order is OK
    //         catcher.reset();
    //         let keys = key_ev_seq(&[
    //             (Key::KEY_B, Press),
    //             (Key::KEY_C, Press),
    //             (Key::KEY_A, Press),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //         Ok(())
    //     }

    //     #[test]
    //     fn test_chord_after_flush() -> Result<()> {
    //         let mut executed = false;
    //         let mut binding = || {
    //             executed = true;
    //             None
    //         };
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             // Break the chord - resulting in a flush
    //             (Key::KEY_G, Press),
    //             // Release the chord
    //             (Key::KEY_B, Release),
    //             (Key::KEY_A, Release),
    //             // Input the full chord
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_C, Press),
    //         ]);
    //         let output_keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_G, Press),
    //             (Key::KEY_B, Release),
    //             (Key::KEY_A, Release),
    //         ]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &output_keys);
    //         assert!(executed);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_chord_after_chord_key() -> Result<()> {
    //         // Test for a bug

    //         let mut executed = false;
    //         let mut binding = || {
    //             executed = true;
    //             None
    //         };
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             (Key::KEY_A, Press),
    //             (Key::KEY_A, Release),
    //             // Input Chord
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_C, Press),
    //             // Release the chord
    //             (Key::KEY_A, Release),
    //             (Key::KEY_B, Release),
    //             (Key::KEY_C, Press),
    //         ]);
    //         let output_keys = key_ev_seq(&[(Key::KEY_A, Press), (Key::KEY_A, Release)]);
    //         input_keys(&mut proc, &keys)?;
    //         dbg!(&proc.chords);
    //         assert_events_eq(&catcher.events.borrow(), &output_keys);
    //         assert!(executed);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_chord_key_pressed_again_while_aborting() -> Result<()> {
    //         // Test for a bug

    //         let mut binding = || None;
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             // Chord
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_C, Press),
    //             // Make sure the total number of releases is more than the
    //             // number of keys in the chord
    //             (Key::KEY_A, Release),
    //             (Key::KEY_B, Release),
    //             (Key::KEY_A, Press),
    //             (Key::KEY_C, Release),
    //             (Key::KEY_A, Release),
    //         ]);
    //         let output_keys = key_ev_seq(&[]);
    //         input_keys(&mut proc, &keys)?;
    //         dbg!(&proc.chords);
    //         assert_events_eq(&catcher.events.borrow(), &output_keys);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_key_repeats_in_chord() -> Result<()> {
    //         let mut executed = false;
    //         let mut binding = || {
    //             executed = true;
    //             None
    //         };
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             // Input the full chord
    //             (Key::KEY_A, Press),
    //             (Key::KEY_B, Press),
    //             (Key::KEY_A, Repeat),
    //             (Key::KEY_A, Repeat),
    //             (Key::KEY_B, Repeat),
    //             (Key::KEY_C, Press),
    //             // And release
    //             (Key::KEY_A, Release),
    //             (Key::KEY_B, Release),
    //             (Key::KEY_C, Release),
    //         ]);
    //         let output_keys = key_ev_seq(&[]);
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &output_keys);
    //         assert!(executed);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_repeat_chord_key_outside_chord() -> Result<()> {
    //         // User should be able to repeat a chord key if it is not building up a chord

    //         let mut binding = || None;
    //         let mut chords = [&mut ChordState::new(
    //             &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //             &mut binding,
    //         )];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[
    //             // Press a key that is part of the chord
    //             (Key::KEY_A, Press),
    //             (Key::KEY_A, Repeat),
    //             (Key::KEY_A, Repeat),
    //             (Key::KEY_A, Repeat),
    //             (Key::KEY_A, Repeat),
    //             (Key::KEY_A, Release),
    //         ]);
    //         let output_keys = &keys;
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &output_keys);
    //         Ok(())
    //     }

    //     #[test]
    //     fn test_single_key_chord() -> Result<()> {
    //         // Just using a single key is a specialization of a chord

    //         let chord_output = key_ev_seq(&[(Key::KEY_G, Press), (Key::KEY_G, Release)]);
    //         let mut binding = || Some(chord_output.clone());
    //         let mut chords = [&mut ChordState::new(&[Key::KEY_A], &mut binding)];

    //         let catcher = Catcher::new();
    //         let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //         let keys = key_ev_seq(&[(Key::KEY_A, Press), (Key::KEY_A, Release)]);
    //         let output_keys = &chord_output;
    //         input_keys(&mut proc, &keys)?;
    //         assert_events_eq(&catcher.events.borrow(), &output_keys);
    //         Ok(())
    //     }

    //     // #[test]
    //     // fn test_chord_timeout() -> Result<()> {
    //     //     // Keys must be passed through if the user did not intend
    //     //     // to use them in a chord.

    //     //     let mut action = || None;
    //     //     let mut chords = [&mut ChordState::new(
    //     //         &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //     //         &mut action,
    //     //     )];

    //     //     let catcher = Catcher::new();
    //     //     let mut proc = Processor::new(&mut chords, catcher.catch());

    //     //     let keys = key_ev_seq(&[
    //     //         // Input partial chord
    //     //         (Key::KEY_A, Press),
    //     //         (Key::KEY_B, Press),
    //     //         // Time out
    //     //     ]);
    //     //     let output_keys = key_ev_seq(&[
    //     //         (Key::KEY_A, Press),
    //     //         (Key::KEY_B, Press),
    //     //     ]);
    //     //     input_keys(&mut proc, &keys)?;
    //     //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     //     Ok(())
    //     // }

    //     // fn test_flush_in_input_order()
}
