use crate::{
    ActionFn, HandlerState, KeyAction, KeyEventHandler, KeyEventValue, NiceKeyInputEvent, ProcView,
};
use anyhow::Result;
use evdev::Key;

pub struct LongPressModifier {
    state: HandlerState,
    orig_key: Key,
    new_key: Key,
    resets: bool,
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
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, Option<HandlerState>)> {
        if pv.event.key != self.orig_key {
            return Ok((KeyAction::PassThrough, None));
        }
        match pv.event.value {
            KeyEventValue::Press => {
                self.state = HandlerState::BuildingUp;
                // Silence the original key until we know if it should be output
                Ok((KeyAction::Hold, Some(self.state)))
            }
            KeyEventValue::QuickRepeat => {
                // Use the new key
                pv.output_kb
                    .emit(&[NiceKeyInputEvent::new(self.new_key, KeyEventValue::Press).into()])?;
                self.state = HandlerState::TearingDown;
                Ok((KeyAction::Discard, Some(self.state)))
            }
            KeyEventValue::Release => {
                if self.state == HandlerState::TearingDown {
                    // New key released
                    pv.output_kb.emit(&[NiceKeyInputEvent::new(
                        self.new_key,
                        KeyEventValue::Release,
                    )
                    .into()])?;
                    self.state = HandlerState::Waiting;
                    Ok((KeyAction::Discard, Some(self.state)))
                } else {
                    // Original key pressed and released (quickly). Flush it out.
                    self.state = HandlerState::Waiting;
                    Ok((KeyAction::PassThrough, Some(self.state)))
                }
            }
            // Repeat will only happen for the new key. It seems safe to just ignore it.
            KeyEventValue::Repeat => Ok((KeyAction::Discard, None)),
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
