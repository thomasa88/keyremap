use std::{
    any::type_name,
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ActionFn, HandlerEvent, HandlerRc, HandlerState, KeyAction, KeyEventHandler, KeyEventValue,
    ProcView, ResetFn,
};
use anyhow::{bail, Result};
use evdev::Key;

pub struct SingleKey {
    key: Key,
    action: ActionFn,
    reset: Option<ResetFn>,
    state: HandlerState,
}

impl Display for SingleKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?} {:?}", type_name::<Self>(), self.key, self.state)
    }
}

impl Debug for SingleKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(type_name::<Self>())
            .field("key", &self.key)
            .field("state", &self.state)
            .finish_non_exhaustive()
    }
}

impl SingleKey {
    pub fn new(key: Key, action: ActionFn, reset: Option<ResetFn>) -> Self {
        Self {
            key,
            action,
            reset,
            state: HandlerState::Waiting,
        }
    }

    pub fn new_rc(key: Key, action: ActionFn, reset: Option<ResetFn>) -> HandlerRc {
        Rc::new(RefCell::new(Self::new(key, action, reset)))
    }
}

impl KeyEventHandler for SingleKey {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, HandlerEvent)> {
        // println!("Check {:?}", self.key);
        if pv.event.key == self.key {
            (self.action)(pv)?;

            // TODO: Check that state transitions are not skipped - i.e. handle double Press events etc. (should not happen)
            use HandlerState::*;
            match self.state {
                Waiting => {
                    self.state = TearingDown;
                    Ok((KeyAction::Discard, HandlerEvent::BuildComplete))
                }
                TearingDown if pv.event.value == KeyEventValue::Release => {
                    self.state = Waiting;
                    Ok((KeyAction::Discard, HandlerEvent::TeardownComplete))
                }
                TearingDown => {
                    // Repeat of some kind
                    Ok((KeyAction::Discard, HandlerEvent::NoEvent))
                }
                BuildingUp => {
                    bail!("Should never get to build state for a single key")
                }
            }
        } else {
            Ok((KeyAction::PassThrough, HandlerEvent::NoEvent))
        }
    }

    fn reset(&mut self) {
        self.reset.as_mut().map(|f| f());
        self.state = HandlerState::Waiting;
    }

    fn get_state(&self) -> HandlerState {
        self.state
    }
}
