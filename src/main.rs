use std::{cell::OnceCell, collections::HashMap, marker::PhantomData, sync::Once, time::Duration, vec};

use anyhow::{Context, Result};
use evdev::{
    uinput::{VirtualDevice, VirtualDeviceBuilder},
    Device, EventType, InputEvent, Key,
};
use num_traits::FromPrimitive;
use scopeguard::{defer, guard};
use tracing_subscriber::{filter, layer};

type ActionFn = Box<dyn Fn(ProcView) -> Result<HandleResult>>;

struct Layer {
    id: usize,
    handler_map: HashMap<KeyEventFilter, Vec<Box<dyn KeyEventHandler>>>,
}

impl Layer {
    // fn new() -> Layer {
    //     Layer {
    //     }
    // }

    fn add_key_press(&mut self, key: Key, action: ActionFn) {
        let filter = KeyEventFilter {
            key_code: key.code(),
        };
        self.handler_map
            .entry(filter)
            .or_default()
            .push(Box::new(KeyPress { filter, action }));
    }
}

struct LayerFactory {
    next_id: usize,
}

impl LayerFactory {
    fn new() -> Self {
        Self { next_id: 0 }
    }

    fn create_layer(&mut self) -> Layer {
        let id = self.next_id;
        self.next_id += 1;
        Layer {
            id,
            handler_map: HashMap::new(),
        }
    }
}

// #[derive(PartialEq, Eq, Hash)]
// enum KeyEventValue {
//     Down,
//     Up,
//     // Both,
// }

#[derive(num_derive::FromPrimitive, Debug, PartialEq, Clone, Copy, Eq, Hash)]
enum KeyEventValue {
    Release = 0,
    Press = 1,
    Repeat = 2,
}

struct NiceKeyInputEvent {
    // pub time: ::timeval,
    // pub type_: ::__u16,
    // pub code: ::__u16,
    // pub value: ::__s32,
    key: Key,
    value: KeyEventValue,
}

impl From<InputEvent> for NiceKeyInputEvent {
    fn from(event: InputEvent) -> Self {
        assert_eq!(event.event_type(), EventType::KEY);
        Self {
            key: Key::new(event.code()),
            value: FromPrimitive::from_i32(event.value()).unwrap(),
        }
    }
}

impl From<NiceKeyInputEvent> for InputEvent {
    fn from(nice: NiceKeyInputEvent) -> Self {
        InputEvent::new(EventType::KEY, nice.key.code(), nice.value as i32)
    }
}

impl NiceKeyInputEvent {
    fn new(key: Key, value: KeyEventValue) -> Self {
        Self { key, value }
    }
}

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
struct KeyEventFilter {
    key_code: u16,
}
struct KeyPress {
    filter: KeyEventFilter,
    action: ActionFn,
}

impl KeyEventHandler for KeyPress {
    fn handle_event(&mut self, pv: ProcView) -> Result<HandleResult> {
        (self.action)(pv)
    }

    fn reset(&mut self) {}
}

trait KeyEventHandler {
    // fn handle_event(&self, proc: &mut Processor) -> Result<HandleResult>;
    fn handle_event(&mut self, pv: ProcView) -> Result<HandleResult>;
    fn reset(&mut self);
}

#[derive(Debug, PartialEq)]
enum HandleResult {
    Handled,
    NotHandled,
}

struct Processor<'a> {
    input_kb: Device,
    output_kb: VirtualDevice,
    //////// Not technically needed to store layers in Processor
    layers: Vec<Layer>,
    // Using a usize, as we cannot store a reference to the layer:
    //  Tried to use reference both in Processor and in a key action closure:
    //   cannot borrow `first_layer` as mutable more than once at a time
    // Possible alternative: Rc<Box<Layer>>?
    active_layer_id: usize,
    // active_layer: Option<&'a mut Layer>,
    _phantom_data: std::marker::PhantomData<&'a ()>, // temp, while refactoring
}

struct ProcView<'v> {
    // Provide full event, to allow for multiple keys handled by same handler as well as timeout handling
    //event: &'v InputEvent,
    event: &'v NiceKeyInputEvent,
    active_layer_id: &'v mut usize,
    output_kb: &'v mut VirtualDevice,
}

impl<'a> ProcView<'a> {
    // fn set_active_layer(layer: &mut Layer) {}
}

impl<'a> Processor<'a> {
    fn new(input_kb: Device, output_kb: VirtualDevice, layers: Vec<Layer>) -> Self {
        assert!(layers.len() > 0);
        let first_layer_id = layers[0].id;
        Self {
            input_kb,
            output_kb,
            layers: layers,
            active_layer_id: first_layer_id,
            _phantom_data: PhantomData,
        }
    }

    fn run(&mut self) -> Result<()> {
        // Make sure that keyboard input is restored on program exit
        let mut input_kb = guard(&mut self.input_kb, |kb| {
            kb.ungrab().unwrap();
        });
        
        // Release all keys that were down during grab, to avoid them being pressed
        // when we ungrab
        // let pressed_keys: Vec<_> = input_kb
        //     .get_key_state()?
        //     .into_iter()
        //     .map(|key| {
        //         NiceKeyInputEvent {
        //             key,
        //             value: KeyEventValue::Release,
        //         }
        //         .into()
        //     })
        //     .collect();
        // self.output_kb.emit(&pressed_keys)?;
        
        // Wait for all current key presses to be released, to avoid them being
        // still pressed after ungrab
        while input_kb.get_key_state()?.into_iter().len() > 0 {
            std::thread::sleep(Duration::from_millis(100));
        }
        input_kb.grab()?;

        loop {
            for event in input_kb.fetch_events()? {
                if event.event_type() != EventType::KEY {
                    self.output_kb.emit(&[event])?;
                    continue;
                }

                let active_layer = self
                    .layers
                    .get_mut(self.active_layer_id)
                    .context("No active layer set")?;
                let mut layer_switch = false;
                let old_layer_id = self.active_layer_id;
                let filter = KeyEventFilter {
                    key_code: event.code(),
                };
                let mut handle_result = HandleResult::NotHandled;
                if let Some(handlers) = &mut active_layer.handler_map.get_mut(&filter) {
                    for handler in &mut **handlers {
                        handle_result = handler.handle_event(ProcView {
                            event: &event.into(),
                            active_layer_id: &mut self.active_layer_id,
                            output_kb: &mut self.output_kb,
                        })?;
                        if self.active_layer_id != old_layer_id {
                            layer_switch = true;
                            break;
                        }
                        if handle_result == HandleResult::Handled {
                            // Only allowing one active event per key
                            // Good or bad?
                            break;
                        }
                    }
                    if layer_switch {
                        // New layer ID already set by callback
                        Self::reset_layer(
                            self.layers
                                .get_mut(old_layer_id)
                                .context("Invalid layer id: ")?,
                        );
                    }
                }
                if handle_result == HandleResult::NotHandled {
                    self.output_kb.emit(&[event])?;
                }
            }
        }
    }

    fn reset_layer(layer: &mut Layer) {
        //// Reset press state from old layer? All except layer button should flush? chords can either just reset - and the keys will then send spurious releases, or they can send press+release for the captured buttons
        for handlers in layer.handler_map.values_mut() {
            for handler in handlers {
                handler.reset();
            }
        }
    }
}

// fn key_event(key: Key, action: KeyEventValue) -> InputEvent {
//     InputEvent::new(EventType::KEY, key.code(), action as i32)
// }

fn main() -> Result<()> {
    let input_kb = Device::open("/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-kbd")?;

    let supported_keys = input_kb.supported_keys().context("Failed to get keys")?;
    let virt_kb = VirtualDeviceBuilder::new()?
        .name("keyremap")
        .with_keys(&supported_keys)?
        .build()?;

    let mut lf = LayerFactory::new();
    const FIRST_LAYER_ID: usize = 0;
    let mut first_layer = lf.create_layer();
    first_layer.id = FIRST_LAYER_ID;

    first_layer.add_key_press(
        Key::KEY_F,
        Box::new(|p| {
            if p.event.value == KeyEventValue::Press {
                *p.active_layer_id = FIRST_LAYER_ID;
                Ok(HandleResult::Handled)
            } else {
                // Just silence the release
                Ok(HandleResult::Handled)
            }
        }),
    );
    first_layer.add_key_press(
        Key::KEY_J,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_U, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
    );

    let layers = vec![first_layer];
    let mut proc = Processor::new(input_kb, virt_kb, layers);

    // let l = proc.layers.get(id).unwrap();
    // proc.set_active_layer(&l);

    proc.run()?;

    Ok(())
}
