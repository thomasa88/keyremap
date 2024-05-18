use std::{borrow::Borrow, future::Future, time::Duration};

use anyhow::Result;
use tokio::task::JoinHandle;

use crate::{ActionFn, HandleResult, KeyEventHandler, KeyEventValue, ProcView};

pub struct QuickRepeatKey {
    action: ActionFn,
    repeat_interval: Duration,
    repeat_task: Option<JoinHandle<()>>,
}

impl QuickRepeatKey {
    pub fn new(repeat_interval: Duration, action: ActionFn) -> Self {
        Self {
            action,
            repeat_interval,
            repeat_task: None,
        }
    }
}

impl KeyEventHandler for QuickRepeatKey {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<HandleResult> {
        // if pv.event.value == KeyEventValue::Press {
        //     tokio::task::LocalSet::new();
        //     ///// stop if already running
        //     self.repeat_task = Some(tokio::task::spawn_local(async {
        //         (self.action)(pv);
        //     }));
        // }
        (self.action)(pv)
    }

    fn reset(&mut self, pv: &mut ProcView) {}
}
