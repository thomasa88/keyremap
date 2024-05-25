use crate::{ActionFn, HandlerState, KeyAction, KeyEventHandler, KeyEventValue, ProcView, ResetFn};
use anyhow::Result;
use evdev::Key;

pub struct SingleKey {
    key: Key,
    action: ActionFn,
    reset: Option<ResetFn>,
    state: HandlerState,
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
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, Option<HandlerState>)> {
        // println!("Check {:?}", self.key);
        if pv.event.key == self.key {
            let down = pv.event.value != KeyEventValue::Release;
            (self.action)(pv)?;
            // TODO: Check that state transitions are not skipped - i.e. handle double Press events etc. (should not happen)
            if down {
                self.state = HandlerState::TearingDown;
            } else {
                self.state = HandlerState::Waiting;
            }
            return Ok((KeyAction::Discard, Some(self.state)));
        }
        Ok((KeyAction::PassThrough, None))
    }

    fn reset(&mut self) {
        // Make sure key (output) is not continued to be seen as pressed down
        ////// This is assuming that the 'press' was propagated and emitted by the handler!
        // if self.down {
        //     pv.output_kb
        //         .emit(&[NiceKeyInputEvent::new(self.key, KeyEventValue::Release).into()]);
        // }
        //self.reset.as_mut().map(|r| r());
        // if self.reset.is_some() {
        //     (self.reset.as_mut().unwrap())()?;
        // }
        self.reset.as_mut().map(|f| f());
        self.state = HandlerState::Waiting;
    }

    fn get_state(&self) -> HandlerState {
        self.state
    }
}
