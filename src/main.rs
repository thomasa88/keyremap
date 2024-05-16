use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    io,
    marker::PhantomData,
    rc::Rc,
    sync::Once,
    time::Duration,
    vec,
};

use anyhow::{Context, Result};
use chord::ChordHandler;
use evdev::{
    uinput::{VirtualDevice, VirtualDeviceBuilder},
    Device, EventType, InputEvent, Key,
};
use num_traits::FromPrimitive;
use scopeguard::{defer, guard};
use tracing_subscriber::{filter, layer};

mod chord;

type ActionFn = Box<dyn Fn(ProcView) -> Result<HandleResult>>;

struct Layer {
    id: usize,
    handler_map: HashMap<KeyEventFilter, Vec<Rc<RefCell<dyn KeyEventHandler>>>>,
    silence_unmapped: bool,
}

impl Layer {
    fn new(id: usize) -> Self {
        Self {
            id,
            handler_map: HashMap::new(),
            silence_unmapped: false,
        }
    }

    fn add_key_press(&mut self, key: Key, action: ActionFn) {
        let filter = KeyEventFilter {
            key_code: key.code(),
        };
        self.handler_map
            .entry(filter)
            .or_default()
            .push(Rc::new(RefCell::new(KeyPress { filter, action })));
    }

    fn add_chord(&mut self, keys: &[Key], action: ActionFn) {
        let key_handler = Rc::new(RefCell::new(ChordHandler::new(keys, action)));
        for key in keys {
            let filter = KeyEventFilter {
                key_code: key.code(),
            };
            self.handler_map
                .entry(filter)
                .or_default()
                .push(key_handler.clone());
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

#[derive(Debug, Clone, Copy)]
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
            // Do we have to release any pressed keys on exit or is that solved by the
            // virtual device being removed?
        });

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
                let silence_unmapped = active_layer.silence_unmapped;
                if let Some(handlers) = &mut active_layer.handler_map.get_mut(&filter) {
                    for handler in &mut **handlers {
                        handle_result = handler.borrow_mut().handle_event(ProcView {
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
                        dbg!(self.active_layer_id);
                    }
                }
                if handle_result == HandleResult::NotHandled && !silence_unmapped {
                    self.output_kb.emit(&[event])?;
                }
            }
        }
    }

    fn reset_layer(layer: &mut Layer) {
        //// Reset press state from old layer? All except layer button should flush? chords can either just reset - and the keys will then send spurious releases, or they can send press+release for the captured buttons
        for handlers in layer.handler_map.values_mut() {
            for handler in handlers {
                handler.borrow_mut().reset();
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

    const HOME_LAYER_ID: usize = 0;
    const NAV_LAYER_ID: usize = 1;
    let mut home_layer = Layer::new(HOME_LAYER_ID);
    let mut nav_layer = Layer::new(NAV_LAYER_ID);

    home_layer.add_key_press(
        Key::KEY_D,
        Box::new(|p| {
            // TODO: Use time instead of repeat/release to skip layer trigger
            if p.event.value == KeyEventValue::Repeat {
                *p.active_layer_id = NAV_LAYER_ID;
                println!("Nav layer");
            } else if p.event.value == KeyEventValue::Release {
                p.output_kb.emit(&[
                    NiceKeyInputEvent::new(Key::KEY_D, KeyEventValue::Press).into(),
                    NiceKeyInputEvent::new(Key::KEY_D, KeyEventValue::Release).into(),
                ])?;
            }
            // Just silence any release or repeat
            Ok(HandleResult::Handled)
        }),
    );
    home_layer.add_chord(
        &[Key::KEY_U, Key::KEY_I],
        Box::new(|pv| {
            pv.output_kb.emit(
                &[
                    NiceKeyInputEvent::new(Key::KEY_SPACE, KeyEventValue::Press),
                    NiceKeyInputEvent::new(Key::KEY_SPACE, KeyEventValue::Release),
                ]
                .map(|e| e.into()),
            )?;
            Ok(HandleResult::Handled)
        }),
    );

    nav_layer.silence_unmapped = true;
    nav_layer.add_key_press(
        Key::KEY_D,
        Box::new(|p| {
            if p.event.value == KeyEventValue::Release {
                *p.active_layer_id = HOME_LAYER_ID;
                println!("Home layer");
            }
            // Just silence any press or repeat
            Ok(HandleResult::Handled)
        }),
    );
    nav_layer.add_key_press(
        Key::KEY_I,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_UP, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
    );
    nav_layer.add_key_press(
        Key::KEY_K,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_DOWN, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
    );
    nav_layer.add_key_press(
        Key::KEY_J,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_LEFT, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
    );
    nav_layer.add_key_press(
        Key::KEY_L,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_RIGHT, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
    );

    let layers = vec![home_layer, nav_layer];
    let mut proc = Processor::new(input_kb, virt_kb, layers);

    proc.run()?;

    Ok(())
}
