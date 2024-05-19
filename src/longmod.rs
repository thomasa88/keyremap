use crate::{ActionFn, HandleResult, KeyEventHandler, KeyEventValue, NiceKeyInputEvent, ProcView};
use anyhow::Result;
use evdev::Key;

pub struct LongPressModifier {
    orig_key: Key,
    new_key: Key,
    active: bool,
    resets: bool,
}

impl LongPressModifier {
    pub fn new(orig_key: Key, new_key: Key) -> Self {
        Self {
            orig_key,
            new_key,
            active: false,
            resets: true,
        }
    }

    pub fn no_reset(mut self) -> Self {
        self.resets = false;
        self
    }
}

impl KeyEventHandler for LongPressModifier {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<HandleResult> {
        match pv.event.value {
            KeyEventValue::Press => {
                // Silence the original key until we know if it should be output
                Ok(HandleResult::Handled)
            }
            KeyEventValue::QuickRepeat => {
                // Use the new key
                self.active = true;
                pv.output_kb
                    .emit(&[NiceKeyInputEvent::new(self.new_key, KeyEventValue::Press).into()])?;
                Ok(HandleResult::Handled)
            }
            KeyEventValue::Release => {
                if self.active {
                    // New key released
                    self.active = false;
                    pv.output_kb.emit(&[NiceKeyInputEvent::new(
                        self.new_key,
                        KeyEventValue::Release,
                    )
                    .into()])?;
                } else {
                    // Original key pressed and released. Output it.
                    pv.output_kb.emit(&[
                        NiceKeyInputEvent::new(self.orig_key, KeyEventValue::Press).into(),
                        NiceKeyInputEvent::new(self.orig_key, KeyEventValue::Release).into(),
                    ])?;
                }
                Ok(HandleResult::Handled)
            }
            // Repeat will only happen for the new key. It seems safe to just ignore it.
            KeyEventValue::Repeat => Ok(HandleResult::Handled),
        }
    }

    fn reset(&mut self, pv: &mut ProcView) {
        // It makes sense to not reset a mod key and keep it until it is released,
        // but then the release must be triggered also in the new layer
        if self.resets && self.active {
            pv.output_kb
                .emit(&[NiceKeyInputEvent::new(self.new_key, KeyEventValue::Release).into()])
                .ok();
        }
    }
}
