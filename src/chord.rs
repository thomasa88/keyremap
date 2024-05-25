use std::collections::BTreeMap;

use anyhow::{ensure, Result};
use evdev::Key;

use crate::{
    ActionFn, HandlerEvent, HandlerState, KeyAction, KeyEventHandler, KeyEventValue, ProcView,
};

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
    state: HandlerState,
    key_states: BTreeMap<Key, KeyState>,
    num_pressed: usize,
}

impl ChordHandler {
    pub fn new(keys: &[Key], action: ActionFn) -> Self {
        Self {
            key_states: keys
                .iter()
                .map(|key| (key.clone(), KeyState::Released))
                .collect(),
            action: action,
            state: HandlerState::Waiting,
            num_pressed: 0,
        }
    }
}

impl KeyEventHandler for ChordHandler {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, HandlerEvent)> {
        let mut key_action;
        let mut handler_event = HandlerEvent::NoEvent;
        let event_key = pv.event.key;

        if self.key_states.contains_key(&event_key) {
            // Key in chord
            
            let mut new_key_state_opt = match pv.event.value {
                KeyEventValue::Press => Some(KeyState::Pressed),
                KeyEventValue::Release => Some(KeyState::Released),
                KeyEventValue::Repeat | KeyEventValue::QuickRepeat => None,
            };

            match self.state {
                HandlerState::Waiting => {
                    key_action = KeyAction::PassThrough;

                    if pv.event.value == KeyEventValue::Press {
                        self.num_pressed += 1;
                        key_action = KeyAction::Hold;
                        handler_event = HandlerEvent::BuildingStarted;
                        self.state = HandlerState::BuildingUp;
                    };
                }
                HandlerState::BuildingUp => {
                    key_action = KeyAction::Hold;
                    ensure!(
                        self.num_pressed > 0,
                        "At least one key should be pressed in building-up state"
                    );

                    if pv.event.value == KeyEventValue::QuickRepeat && self.num_pressed == 1 {
                        // Allow a normal repeat of a key if it is the first of the chord
                        // The chord will be broken

                        // Should not really need to check that the key is pressed,
                        // as repeat implies pressed
                        ensure!(*self.key_states.get(&event_key).unwrap() == KeyState::Pressed);

                        //////// clean up
                        new_key_state_opt = Some(KeyState::Released);
                        self.reset();

                        key_action = KeyAction::PassThrough;
                        handler_event = HandlerEvent::Aborted;
                        self.state = HandlerState::Waiting;
                    } else if pv.event.value == KeyEventValue::Press {
                        self.num_pressed += 1;

                        if self.num_pressed == self.key_states.len() {
                            // Chord completed!
                            key_action = KeyAction::Discard;
                            handler_event = HandlerEvent::BuildComplete;
                            self.state = HandlerState::TearingDown;
                            (self.action)(pv)?;
                        }
                    } else if pv.event.value == KeyEventValue::Release {
                        // Chord is broken
                        self.num_pressed -= 1;

                        //////// clean up
                        new_key_state_opt = Some(KeyState::Released);
                        self.reset();

                        key_action = KeyAction::PassThrough;
                        handler_event = HandlerEvent::Aborted;
                        self.state = HandlerState::Waiting;
                    }
                }
                HandlerState::TearingDown => {
                    key_action = KeyAction::Discard;
                    ensure!(
                        self.num_pressed > 0,
                        "At least one key should be pressed in tear-down state"
                    );
                    // When releasing a chord, we should still silent the key events
                    if pv.event.value == KeyEventValue::Release {
                        self.num_pressed -= 1;
                        if self.num_pressed == 0 {
                            handler_event = HandlerEvent::TeardownComplete;
                            self.state = HandlerState::Waiting;
                        }
                    } else if pv.event.value == KeyEventValue::Press {
                        // Must count this even when releasing the chord,
                        // as we do num_pressed -=1 in the other branch.
                        // Alternative: Ignore pressed event and only decrement
                        // if the old state is released.
                        self.num_pressed += 1;
                    }
                }
            }

            // Update with the latest state after processing with the old state
            if let Some(new_key_state) = new_key_state_opt {
                *self.key_states.get_mut(&event_key).unwrap() = new_key_state;
            }
        } else {
            // Key not in chord
            key_action = KeyAction::PassThrough;
            if self.state == HandlerState::BuildingUp {
                // Key outside of active chord. Chord is broken!

                //////// clean up
                self.reset();

                handler_event = HandlerEvent::Aborted;
                self.state = HandlerState::Waiting;
            }
        }
        Ok((key_action, handler_event))
    }

    fn reset(&mut self) {
        self.key_states
            .values_mut()
            .for_each(|v| *v = KeyState::Released);
        self.num_pressed = 0;
        self.state = HandlerState::Waiting;
    }

    fn get_state(&self) -> HandlerState {
        self.state
    }
}

#[cfg(test)]
mod tests {
    use std::iter::zip;

    use evdev::EventType;
    use itertools::Itertools;

    use super::*;
    use crate::HandlerState::*;
    use crate::KeyEventValue::*;
    use crate::VirtualDevice;

    fn panic_on_call() -> ActionFn {
        Box::new(|_: &mut ProcView<'_>| panic!("Chord executed"))
    }

    fn key_ev_seq(ev_infos: &[(Key, KeyEventValue)]) -> Vec<InputEvent> {
        ev_infos
            .into_iter()
            .map(|(k, v)| InputEvent::new(EventType::KEY, k.code(), *v as i32))
            .collect()
    }

    fn handle_seq(
        chord: &mut ChordHandler,
        seq: &[(Key, KeyEventValue, HandlerState)],
    ) -> Vec<InputEvent> {
        let mut virt_kb = VirtualDevice::default();
        for (i, (key, value, exp_result)) in seq.iter().enumerate() {
            let result = chord
                .handle_event(&mut ProcView {
                    event: &NiceKeyInputEvent {
                        key: *key,
                        value: *value,
                    },
                    active_layer_id: &mut 0,
                    output_kb: &mut virt_kb,
                })
                .unwrap();
            assert_eq!(
                result,
                *exp_result,
                "Mismatching handle result at index {i} (key {})",
                key.code()
            );
        }
        virt_kb.log
    }

    fn assert_event_eq(lhs: &InputEvent, rhs: &InputEvent) {
        assert_eq!(lhs.event_type(), rhs.event_type());
        assert_eq!(lhs.code(), rhs.code());
        assert_eq!(lhs.value(), rhs.value());
    }

    fn assert_events_eq<T, U>(a: T, b: U)
    where
        T: AsRef<[InputEvent]>,
        U: AsRef<[InputEvent]>,
    {
        let a = a.as_ref();
        let b = b.as_ref();
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

    fn events_str(events: &[InputEvent]) -> String {
        ["[", &events.iter().map(|e| event_str(e)).join(",\n"), "]"].concat()
    }

    fn event_str(event: &InputEvent) -> String {
        format!(
            "InputEvent {{ kind: {:?}, action: {:?}, ...}}",
            event.kind(),
            <KeyEventValue as FromPrimitive>::from_i32(event.value())
        )
    }

    #[test]
    fn test_chord_first_key_press() -> Result<()> {
        let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], panic_on_call());

        assert!(handle_seq(&mut chord, &[(Key::KEY_A, Press, Handled)]).is_empty());

        Ok(())
    }

    #[test]
    fn test_chord_second_key_press() -> Result<()> {
        let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], panic_on_call());

        assert!(handle_seq(
            &mut chord,
            &[(Key::KEY_A, Press, Handled), (Key::KEY_B, Press, Handled)],
        )
        .is_empty());

        Ok(())
    }

    #[test]
    fn test_chord_broken_by_other_key() -> Result<()> {
        // All keys should flush if the chord is broken

        let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], panic_on_call());

        assert_events_eq(
            handle_seq(
                &mut chord,
                &[
                    (Key::KEY_A, Press, Handled),
                    (Key::KEY_B, Press, Handled),
                    (Key::KEY_G, Release, Handled),
                ],
            ),
            key_ev_seq(&[
                (Key::KEY_A, Press),
                (Key::KEY_B, Press),
                (Key::KEY_G, Release),
            ]),
        );
        Ok(())
    }

    #[test]
    fn test_chord_broken_by_release() -> Result<()> {
        // All keys should flush if the chord is broken

        let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], panic_on_call());

        assert_events_eq(
            handle_seq(
                &mut chord,
                &[
                    (Key::KEY_A, Press, Handled),
                    (Key::KEY_B, Press, Handled),
                    (Key::KEY_A, Release, Handled),
                ],
            ),
            key_ev_seq(&[
                (Key::KEY_A, Press),
                (Key::KEY_B, Press),
                (Key::KEY_A, Release),
            ]),
        );
        Ok(())
    }

    // #[test]
    // fn test_passthrough_while_flushing() -> Result<()> {
    //     // All events should be passed through when a chord is breaking up

    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], panic_on_call());

    //     let keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         // Break the chord before completing it
    //         (Key::KEY_G, Press),
    //         // And continue typing, even letters from the chord - as long as some parts of the chord is still pressed
    //         (Key::KEY_B, Release),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_B, Release),
    //         (Key::KEY_I, Press),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &keys);
    //     Ok(())
    // }

    // #[test]
    // fn test_complete_chord() -> Result<()> {
    //     let catcher = Catcher::new();
    //     const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

    //     let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //     let keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_C, Press),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //     // User releases the chord press
    //     let keys = key_ev_seq(&[
    //         (Key::KEY_A, Release),
    //         (Key::KEY_C, Release),
    //         (Key::KEY_B, Release),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //     Ok(())
    // }

    // #[test]
    // fn test_passthrough_after_chord() -> Result<()> {
    //     let catcher = Catcher::new();
    //     const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

    //     let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //     let keys = key_ev_seq(&[
    //         // Press chord
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_C, Press),
    //         // We allow new presses before the chord is fully release
    //         (Key::KEY_G, Press),
    //         // Release chord
    //         (Key::KEY_A, Release),
    //         (Key::KEY_B, Release),
    //         (Key::KEY_C, Release),
    //         // Key after release
    //         (Key::KEY_G, Release),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(
    //         &catcher.events.borrow(),
    //         &key_ev_seq(&[CHORD_OUTPUT, &[(Key::KEY_G, Press), (Key::KEY_G, Release)]].concat()),
    //     );

    //     Ok(())
    // }

    // #[test]
    // fn test_chord_after_chord() -> Result<()> {
    //     let catcher = Catcher::new();
    //     const CHORD_OUTPUT: &[(Key, KeyAction)] = &[(Key::KEY_H, Press), (Key::KEY_H, Release)];

    //     let mut binding = || Some(key_ev_seq(CHORD_OUTPUT));
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let mut proc = Processor::new(&mut chords, catcher.catch_emit());

    //     let keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_C, Press),
    //         (Key::KEY_A, Release),
    //         (Key::KEY_B, Release),
    //         (Key::KEY_C, Release),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //     // Any input order is OK
    //     catcher.reset();
    //     let keys = key_ev_seq(&[
    //         (Key::KEY_B, Press),
    //         (Key::KEY_C, Press),
    //         (Key::KEY_A, Press),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &key_ev_seq(CHORD_OUTPUT));

    //     Ok(())
    // }

    // #[test]
    // fn test_chord_after_flush() -> Result<()> {
    //     let mut executed = false;
    //     let mut binding = || {
    //         executed = true;
    //         None
    //     };
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         // Break the chord - resulting in a flush
    //         (Key::KEY_G, Press),
    //         // Release the chord
    //         (Key::KEY_B, Release),
    //         (Key::KEY_A, Release),
    //         // Input the full chord
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_C, Press),
    //     ]);
    //     let output_keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_G, Press),
    //         (Key::KEY_B, Release),
    //         (Key::KEY_A, Release),
    //     ]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     assert!(executed);
    //     Ok(())
    // }

    // #[test]
    // fn test_chord_after_chord_key() -> Result<()> {
    //     // Test for a bug

    //     let mut executed = false;
    //     let mut binding = || {
    //         executed = true;
    //         None
    //     };
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_A, Release),
    //         // Input Chord
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_C, Press),
    //         // Release the chord
    //         (Key::KEY_A, Release),
    //         (Key::KEY_B, Release),
    //         (Key::KEY_C, Press),
    //     ]);
    //     let output_keys = key_ev_seq(&[(Key::KEY_A, Press), (Key::KEY_A, Release)]);
    //     input_keys(&mut proc, &keys)?;
    //     dbg!(&proc.chords);
    //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     assert!(executed);
    //     Ok(())
    // }

    // #[test]
    // fn test_chord_key_pressed_again_while_aborting() -> Result<()> {
    //     // Test for a bug

    //     let mut binding = || None;
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let keys = key_ev_seq(&[
    //         // Chord
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_C, Press),
    //         // Make sure the total number of releases is more than the
    //         // number of keys in the chord
    //         (Key::KEY_A, Release),
    //         (Key::KEY_B, Release),
    //         (Key::KEY_A, Press),
    //         (Key::KEY_C, Release),
    //         (Key::KEY_A, Release),
    //     ]);
    //     let output_keys = key_ev_seq(&[]);
    //     input_keys(&mut proc, &keys)?;
    //     dbg!(&proc.chords);
    //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     Ok(())
    // }

    // #[test]
    // fn test_key_repeats_in_chord() -> Result<()> {
    //     let mut executed = false;
    //     let mut binding = || {
    //         executed = true;
    //         None
    //     };
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let keys = key_ev_seq(&[
    //         // Input the full chord
    //         (Key::KEY_A, Press),
    //         (Key::KEY_B, Press),
    //         (Key::KEY_A, Repeat),
    //         (Key::KEY_A, Repeat),
    //         (Key::KEY_B, Repeat),
    //         (Key::KEY_C, Press),
    //         // And release
    //         (Key::KEY_A, Release),
    //         (Key::KEY_B, Release),
    //         (Key::KEY_C, Release),
    //     ]);
    //     let output_keys = key_ev_seq(&[]);
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     assert!(executed);
    //     Ok(())
    // }

    // #[test]
    // fn test_repeat_chord_key_outside_chord() -> Result<()> {
    //     // User should be able to repeat a chord key if it is not building up a chord

    //     let mut binding = || None;
    //     let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], &mut binding);

    //     let keys = key_ev_seq(&[
    //         // Press a key that is part of the chord
    //         (Key::KEY_A, Press),
    //         (Key::KEY_A, Repeat),
    //         (Key::KEY_A, Repeat),
    //         (Key::KEY_A, Repeat),
    //         (Key::KEY_A, Repeat),
    //         (Key::KEY_A, Release),
    //     ]);
    //     let output_keys = &keys;
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     Ok(())
    // }

    // #[test]
    // fn test_single_key_chord() -> Result<()> {
    //     // Just using a single key is a specialization of a chord

    //     let chord_output = key_ev_seq(&[(Key::KEY_G, Press), (Key::KEY_G, Release)]);
    //     let mut binding = || Some(chord_output.clone());
    //     let mut chord = ChordHandler::new(&[Key::KEY_A], &mut binding);

    //     let keys = key_ev_seq(&[(Key::KEY_A, Press), (Key::KEY_A, Release)]);
    //     let output_keys = &chord_output;
    //     input_keys(&mut proc, &keys)?;
    //     assert_events_eq(&catcher.events.borrow(), &output_keys);
    //     Ok(())
    // }

    // #[test]
    // fn test_chord_timeout() -> Result<()> {
    //     // Keys must be passed through if the user did not intend
    //     // to use them in a chord.

    //     let mut action = || None;
    //     let mut chord = ChordHandler::new(
    //         &[Key::KEY_A, Key::KEY_B, Key::KEY_C],
    //         &mut action,
    //     );

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

    #[test]
    fn test_flush_in_input_order() -> Result<()> {
        // User's writing will be messed up if the keys are not flushed in the order
        // in which they were input.

        let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], panic_on_call());

        assert_events_eq(
            handle_seq(
                &mut chord,
                &[
                    (Key::KEY_A, Press, Handled),
                    (Key::KEY_B, Press, Handled),
                    (Key::KEY_A, Release, Handled),
                ],
            ),
            key_ev_seq(&[
                (Key::KEY_A, Press),
                (Key::KEY_B, Press),
                (Key::KEY_A, Release),
            ]),
        );

        let mut chord = ChordHandler::new(&[Key::KEY_A, Key::KEY_B, Key::KEY_C], panic_on_call());

        assert_events_eq(
            handle_seq(
                &mut chord,
                &[
                    (Key::KEY_B, Press, Handled),
                    (Key::KEY_A, Press, Handled),
                    (Key::KEY_A, Release, Handled),
                ],
            ),
            key_ev_seq(&[
                (Key::KEY_B, Press),
                (Key::KEY_A, Press),
                (Key::KEY_A, Release),
            ]),
        );
        Ok(())
    }
}
