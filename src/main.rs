use std::{
    cell::{OnceCell, RefCell},
    collections::{HashMap, VecDeque},
    io,
    marker::PhantomData,
    rc::Rc,
    sync::Once,
    time::Duration,
    vec,
};

use anyhow::{Context, Result};
use chord::ChordHandler;
use longmod::LongPressModifier;
use evdev::{AttributeSetRef, Device, EventType, InputEvent, Key};
use futures::stream::FuturesUnordered;
use num_traits::FromPrimitive;
use scopeguard::{defer, guard};
use single_key::SingleKey;
use tokio::{select, task::JoinSet, time::Instant};
use tracing::event;
use tracing_subscriber::{filter, layer};

mod chord;
mod longmod;
mod qrkey;
mod single_key;

#[cfg(not(test))]
use evdev::uinput::{VirtualDevice, VirtualDeviceBuilder};

// Primitive mocking of VirtualDevice
#[cfg(test)]
#[derive(Default)]
struct VirtualDevice {
    log: Vec<InputEvent>,
}

#[cfg(test)]
impl VirtualDevice {
    fn emit(&mut self, messages: &[InputEvent]) -> Result<(), std::io::Error> {
        self.log.extend(messages);
        Ok(())
    }
}

#[cfg(test)]
struct VirtualDeviceBuilder {}

#[cfg(test)]
impl VirtualDeviceBuilder {
    fn new() -> Result<Self> {
        Ok(Self {})
    }

    fn name(&mut self, _: &str) -> &mut Self {
        self
    }

    fn with_keys(&mut self, _: &AttributeSetRef<Key>) -> Result<&mut Self> {
        Ok(self)
    }

    fn build(&mut self) -> Result<VirtualDevice> {
        Ok(VirtualDevice::default())
    }
}

type ActionFn = Box<dyn FnMut(&mut ProcView) -> Result<HandleResult>>;

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

    fn add_key_press(&mut self, key: Key, action: ActionFn, reset: Option<ActionFn>) {
        let filter = KeyEventFilter {
            key_code: key.code(),
        };
        self.handler_map
            .entry(filter)
            .or_default()
            .push(Rc::new(RefCell::new(SingleKey::new(action, reset))));
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

    fn add_handler(&mut self, key: Key, key_handler: impl KeyEventHandler + 'static) {
        self.handler_map
            .entry(KeyEventFilter {
                key_code: key.code(),
            })
            .or_default()
            .push(Rc::new(RefCell::new(key_handler)));
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
    /// Injected event, that fires more quickly than Repeat. It is only fired once per Press.
    QuickRepeat = 99,
}

#[derive(Debug, Clone, Copy)]
struct NiceKeyInputEvent {
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

trait KeyEventHandler {
    // fn handle_event(&self, proc: &mut Processor) -> Result<HandleResult>;
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<HandleResult>;
    fn reset(&mut self, pv: &mut ProcView);
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
    event: &'v NiceKeyInputEvent,
    active_layer_id: &'v mut usize,
    output_kb: &'v mut VirtualDevice,
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

    // run() consumes self since Device.into_event_stream() needs to consume Device
    // Alternative: Store Device in an Option and use take, or pass in input_kb separately
    async fn run(mut self) -> Result<()> {
        let mut unguarded_input_stream = self.input_kb.into_event_stream()?;
        // Make sure that keyboard input is restored on program exit
        let mut input_stream = guard(&mut unguarded_input_stream, |stream| {
            stream.device_mut().ungrab().unwrap();
            // Do we have to release any pressed keys on exit or is that solved by the
            // virtual device being removed?
        });

        let input_dev = input_stream.device_mut();
        // Wait for all current key presses to be released, to avoid them being
        // still pressed after ungrab
        while input_dev.get_key_state()?.into_iter().len() > 0 {
            std::thread::sleep(Duration::from_millis(100));
        }
        input_dev.grab()?;

        let mut quick_repeats = VecDeque::<(Key, Instant)>::new();
        // let mut w = None;
        // let mut futures = JoinSet::new();
        // let mut futures = FuturesUnordered::new();
        // futures.push(input_stream.next_event());
        // let mut next_quick_repeat = None;
        loop {
            // let next_quick_repeat = quick_repeats.pop_front();
            // let mut quick_repeat_wait = if let Some((key, time)) = next_quick_repeat {
            //     Some(async move {
            //         tokio::time::sleep_until(time).await;
            //         NiceKeyInputEvent {
            //             key,
            //             value: KeyEventValue::QuickRepeat,
            //         }
            //         .into()
            //     })
            // } else {
            //     None
            // };

            // let event = if let Some(ww) = quick_repeat_wait.take() {
            //     select! {
            //         val = input_stream.next_event() => val?,
            //         val = ww => val,
            //     }
            // } else {
            //     select! {
            //         val = input_stream.next_event() => val?,
            //     }
            // };

            // FuturesUnordered or some kind of stream might be an alternative to the multible branches
            // with select!.
            let event = if let Some((key, time)) = quick_repeats.front() {
                let quick_repeat_wait = async move {
                    tokio::time::sleep_until(*time).await;
                    NiceKeyInputEvent {
                        key: *key,
                        value: KeyEventValue::QuickRepeat,
                    }
                    .into()
                };
                select! {
                    val = input_stream.next_event() => val?,
                    val = quick_repeat_wait => val,
                }
            } else {
                select! {
                    val = input_stream.next_event() => val?,
                }
            };
            if event.value() == KeyEventValue::QuickRepeat as i32 {
                quick_repeats.pop_front();
            }
            // if event.is_none() {
            //     continue;
            // }
            // let event = event.unwrap();
            // tokio::time::timeout(duration, future)
            // let event = input_stream.next_event().await?;
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
                    handle_result = handler.borrow_mut().handle_event(&mut ProcView {
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
                        &mut ProcView {
                            event: &event.into(),
                            active_layer_id: &mut self.active_layer_id,
                            output_kb: &mut self.output_kb,
                        },
                    );
                    dbg!(self.active_layer_id);
                }
            }

            if event.value() == KeyEventValue::Release as i32 {
                ///// fix
                // Clean any quick repeats
                quick_repeats.retain(|(key, _time)| key.code() != event.code());
            }

            if handle_result == HandleResult::NotHandled
                && !silence_unmapped
                && event.value() != KeyEventValue::QuickRepeat as i32
            {
                self.output_kb.emit(&[event])?;
            } else if handle_result == HandleResult::Handled
                && event.value() == KeyEventValue::Press as i32
            {
                quick_repeats.push_back((
                    Key::new(event.code()),
                    Instant::now() + Duration::from_millis(200),
                ));
            }
        }
    }

    fn reset_layer(layer: &mut Layer, pv: &mut ProcView) {
        //// Reset press state from old layer? All except layer button should flush? chords can either just reset - and the keys will then send spurious releases, or they can send press+release for the captured buttons
        for handlers in layer.handler_map.values_mut() {
            for handler in handlers {
                handler.borrow_mut().reset(pv);
            }
        }
    }
}

// fn key_event(key: Key, action: KeyEventValue) -> InputEvent {
//     InputEvent::new(EventType::KEY, key.code(), action as i32)
// }

// Tokio spawn() requires arguments to be Sync, even though it is only used on
// one thread. Using tokio::task::LocalSet instead - to still have sleep.
// Another alternative is async_executor::LocalExecutor (but can we sleep using it?)
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
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
        Key::KEY_F,
        Box::new(|pv| {
            if pv.event.value == KeyEventValue::QuickRepeat {
                *pv.active_layer_id = NAV_LAYER_ID;
                println!("Nav layer");
            } else if pv.event.value == KeyEventValue::Release {
                // Release happenend before layer switch -> cancel layer key
                pv.output_kb.emit(&[
                    NiceKeyInputEvent::new(Key::KEY_F, KeyEventValue::Press).into(),
                    NiceKeyInputEvent::new(Key::KEY_F, KeyEventValue::Release).into(),
                ])?;
            }
            Ok(HandleResult::Handled)
        }),
        None,
    );
    // let mut alt_alt = false;
    // home_layer.add_key_press(
    //     Key::KEY_D,
    //     Box::new(move |pv| {
    //         if pv.event.value == KeyEventValue::QuickRepeat {
    //             alt_alt = true;
    //             pv.output_kb.emit(&[NiceKeyInputEvent::new(
    //                 Key::KEY_LEFTALT,
    //                 KeyEventValue::Press,
    //             )
    //             .into()])?;
    //             Ok(HandleResult::Handled)
    //         } else if alt_alt
    //             && (pv.event.value == KeyEventValue::Release
    //                 || pv.event.value == KeyEventValue::Repeat)
    //         {
    //             pv.output_kb
    //                 .emit(&[NiceKeyInputEvent::new(Key::KEY_LEFTALT, pv.event.value).into()])?;

    //             if pv.event.value == KeyEventValue::Release {
    //                 alt_alt = false;
    //             }

    //             Ok(HandleResult::Handled)
    //         } else {
    //             Ok(HandleResult::NotHandled)
    //         }
    //     }),
    //     None,
    // );
    home_layer.add_handler(
        Key::KEY_D,
        LongPressModifier::new(Key::KEY_D, Key::KEY_LEFTALT).no_reset(),
    );
    home_layer.add_handler(
        Key::KEY_S,
        LongPressModifier::new(Key::KEY_S, Key::KEY_LEFTCTRL).no_reset(),
    );
    nav_layer.add_handler(
        Key::KEY_D,
        LongPressModifier::new(Key::KEY_D, Key::KEY_LEFTALT).no_reset(),
    );
    nav_layer.add_handler(
        Key::KEY_S,
        LongPressModifier::new(Key::KEY_S, Key::KEY_LEFTCTRL).no_reset(),
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
        Key::KEY_F,
        Box::new(|p| {
            if p.event.value == KeyEventValue::Release {
                *p.active_layer_id = HOME_LAYER_ID;
                println!("Home layer");
            }
            // Just silence any press or repeat
            Ok(HandleResult::Handled)
        }),
        None,
    );
    nav_layer.add_key_press(
        Key::KEY_LEFTALT,
        Box::new(|pv| {
            pv.output_kb.emit(&[(*pv.event).into()])?;
            Ok(HandleResult::Handled)
        }),
        None,
    );
    nav_layer.add_key_press(
        Key::KEY_I,
        Box::new(|pv| {
            pv.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_UP, pv.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
        Some(Box::new(|pv| {
            // This is needed to make sure the key is released when the layer is switched.
            // Otherwise the remapped key will be continue to be pressed.
            ////// only emit this if the key is currently pressed - need to save state
            pv.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_UP, KeyEventValue::Release).into()])?;
            Ok(HandleResult::Handled)
        })),
    );
    nav_layer.add_key_press(
        Key::KEY_K,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_DOWN, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
        Some(Box::new(|pv| {
            ////// only emit this if the key is currently pressed - need to save state
            pv.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_DOWN, KeyEventValue::Release).into()])?;
            Ok(HandleResult::Handled)
        })),
    );
    nav_layer.add_key_press(
        Key::KEY_J,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_LEFT, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
        Some(Box::new(|pv| {
            ////// only emit this if the key is currently pressed - need to save state
            pv.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_LEFT, KeyEventValue::Release).into()])?;
            Ok(HandleResult::Handled)
        })),
    );
    nav_layer.add_key_press(
        Key::KEY_L,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_RIGHT, p.event.value).into()])?;
            Ok(HandleResult::Handled)
        }),
        Some(Box::new(|pv| {
            ////// only emit this if the key is currently pressed - need to save state
            pv.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_RIGHT, KeyEventValue::Release).into()])?;
            Ok(HandleResult::Handled)
        })),
    );

    let layers = vec![home_layer, nav_layer];
    let proc = Processor::new(input_kb, virt_kb, layers);

    proc.run().await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    // #[test]
    // fn test_free_key() -> Result<()> {
    //     let catcher = Catcher::new();
    //     let mut proc = Processor::new(&mut [], catcher.catch_emit());

    //     let keys = key_ev_seq(&[
    //         (Key::KEY_A, Press),
    //         (Key::KEY_A, Repeat),
    //         (Key::KEY_A, Release),
    //     ]);
    //     input_keys(&mut proc, &keys)?;

    //     assert_events_eq(&catcher.events.borrow(), &keys);
    //     Ok(())
    // }
}
