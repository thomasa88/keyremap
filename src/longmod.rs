use std::{
    any::type_name,
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ActionFn, HandlerEvent, HandlerBox, HandlerState, KeyAction, KeyEventHandler, KeyEventValue,
    NiceKeyInputEvent, ProcView,
};
use anyhow::Result;
use evdev::Key;

pub struct LongPressModifier {
    state: HandlerState,
    orig_key: Key,
    new_key: Action,
    resets: bool,
}
pub enum Action {
    Key(Key),
    Fn(ActionFn),
}

impl Display for LongPressModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {:?} {:?}",
            type_name::<Self>(),
            self.orig_key,
            self.state
        )
    }
}

impl Debug for LongPressModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(type_name::<Self>())
            .field("orig_key", &self.orig_key)
            // .field("new_key", &self.new_key)
            .field("state", &self.state)
            .finish_non_exhaustive()
    }
}

impl LongPressModifier {
    pub fn new(orig_key: Key, new_key: Action) -> Self {
        Self {
            state: HandlerState::Waiting,
            orig_key,
            new_key,
            resets: true,
        }
    }

    // pub fn new_rc(orig_key: Key, new_key: Action) -> HandlerBox {
    //     Rc::new(RefCell::new(Self::new(orig_key, new_key)))
    // }

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
                match &mut self.new_key {
                    Action::Key(key) => {
                        pv.output_kb
                            .emit(&[NiceKeyInputEvent::new(*key, KeyEventValue::Press).into()])?
                    }
                    Action::Fn(f) => (f)(pv)?,
                }
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
                    match &mut self.new_key {
                        Action::Key(key) => pv.output_kb.emit(&[NiceKeyInputEvent::new(
                            *key,
                            KeyEventValue::Release,
                        )
                        .into()])?,
                        Action::Fn(f) => (f)(pv)?,
                    }
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
