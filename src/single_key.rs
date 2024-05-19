use anyhow::Result;
use crate::{ActionFn, HandleResult, KeyEventHandler, KeyEventValue, ProcView};

pub struct SingleKey {
    // filter: KeyEventFilter,
    action: ActionFn,
    reset: Option<ActionFn>,
    down: bool,
}

impl SingleKey {
    pub fn new(action: ActionFn, reset: Option<ActionFn>) -> Self {
        Self {
            action,
            reset,
            down: false,
        }
    }
}

impl KeyEventHandler for SingleKey {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<HandleResult> {
        self.down = pv.event.value != KeyEventValue::Release;
        (self.action)(pv)
    }

    fn reset(&mut self, pv: &mut ProcView) {
        // Make sure key (output) is not continued to be seen as pressed down
        ////// This is assuming that the 'press' was propagated and emitted by the handler!
        // if self.down {
        //     pv.output_kb
        //         .emit(&[NiceKeyInputEvent::new(self.key, KeyEventValue::Release).into()]);
        // }
        self.reset.as_mut().map(|r| r(pv));
    }
}
