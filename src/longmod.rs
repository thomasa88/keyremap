use std::{any::type_name, fmt::Debug};

use crate::{
    ActionFn, HandlerEvent, HandlerState, KeyAction, KeyEventHandler, KeyEventValue,
    NiceKeyInputEvent, ProcView,
};
use anyhow::Result;
use evdev::Key;

pub struct LongPressModifier {
    state: HandlerState,
    orig_key: Key,
    new_key: Key,
    resets: bool,
}

impl Debug for LongPressModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(type_name::<Self>())
            .field("orig_key", &self.orig_key)
            .field("new_key", &self.new_key)
            .field("state", &self.state)
            .finish_non_exhaustive()
    }
}

impl LongPressModifier {
    pub fn new(orig_key: Key, new_key: Key) -> Self {
        Self {
            state: HandlerState::Waiting,
            orig_key,
            new_key,
            resets: true,
        }
    }

    pub fn no_reset(mut self) -> Self {
        self.resets = false;
        self
    }
}

impl KeyEventHandler for LongPressModifier {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, HandlerEvent)> {
        if pv.event.key != self.orig_key {
            return Ok((KeyAction::PassThrough, HandlerEvent::NoEvent));
        }
        match pv.event.value {
            KeyEventValue::Press => {
                self.state = HandlerState::BuildingUp;
                // Silence the original key until we know if it should be output
                Ok((KeyAction::Hold, HandlerEvent::BuildingStarted))
            }
            KeyEventValue::QuickRepeat => {
                // Use the new key
                pv.output_kb
                    .emit(&[NiceKeyInputEvent::new(self.new_key, KeyEventValue::Press).into()])?;
                if self.state != HandlerState::TearingDown {
                    self.state = HandlerState::TearingDown;
                    Ok((KeyAction::Discard, HandlerEvent::BuildComplete))
                } else {
                    Ok((KeyAction::Discard, HandlerEvent::NoEvent))
                }
            }
            KeyEventValue::Release => match self.state {
                HandlerState::TearingDown => {
                    // New key released
                    pv.output_kb.emit(&[NiceKeyInputEvent::new(
                        self.new_key,
                        KeyEventValue::Release,
                    )
                    .into()])?;
                    self.state = HandlerState::Waiting;
                    Ok((KeyAction::Discard, HandlerEvent::TeardownComplete))
                }
                HandlerState::BuildingUp => {
                    // Original key pressed and released (quickly). Flush it out.
                    self.state = HandlerState::Waiting;
                    Ok((KeyAction::PassThrough, HandlerEvent::Aborted))
                }
                HandlerState::Waiting => {
                    // Original key released without having triggered the long key press modifier
                    Ok((KeyAction::PassThrough, HandlerEvent::NoEvent))
                }
            },
            // Repeat will only happen for the new key. It seems safe to just ignore it.
            KeyEventValue::Repeat => Ok((KeyAction::Discard, HandlerEvent::NoEvent)),
        }
    }

    fn reset(&mut self) {
        // It makes sense to not reset a mod key and keep it until it is released,
        // but then the release must be triggered also in the new layer
        // if self.resets && self.active {
        //     pv.output_kb
        //         .emit(&[NiceKeyInputEvent::new(self.new_key, KeyEventValue::Release).into()])
        //         .ok();
        // }
        self.state = HandlerState::Waiting;
    }

    fn get_state(&self) -> HandlerState {
        self.state
    }
}
