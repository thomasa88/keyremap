// Copyright (C) 2024  Thomas Axelsson
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::{
    any::type_name,
    fmt::{Debug, Display},
};

use crate::{
    ActionFn, HandlerEvent, HandlerState, KeyAction, KeyEventHandler, KeyEventValue,
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
