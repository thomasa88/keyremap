mod chord;
mod longmod;
mod single_key;

use std::fmt::Debug;
use std::{
    cell::{OnceCell, Ref, RefCell},
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
    io,
    marker::PhantomData,
    rc::Rc,
    sync::Once,
    time::Duration,
    vec,
};

use anyhow::{ensure, Context, Result};
use chord::ChordHandler;
use evdev::{AttributeSetRef, Device, EventStream, EventType, InputEvent, Key};
use futures::stream::FuturesUnordered;
use longmod::LongPressModifier;
use num_traits::FromPrimitive;
use scopeguard::{defer, guard};
use single_key::SingleKey;
use tokio::{pin, runtime::Handle, select, task::JoinSet, time::Instant};
use tracing::event;
use tracing_subscriber::{filter, layer};
use Iterator;

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

type ActionFn = Box<dyn FnMut(&mut ProcView) -> Result<()>>;
type ResetFn = Box<dyn FnMut()>;
type HandlerRc = Rc<RefCell<dyn KeyEventHandler>>;

struct Layer {
    id: usize,
    handler_map: HashMap<KeyEventFilter, Vec<HandlerRc>>,
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

    // fn add_key_press(&mut self, key: Key, action: ActionFn, reset: Option<ResetFn>) {
    //     let filter = KeyEventFilter {
    //         key_code: key.code(),
    //     };
    //     self.handler_map
    //         .entry(filter)
    //         .or_default()
    //         .push(Rc::new(RefCell::new(SingleKey::new(key, action, reset))));
    // }

    // fn add_chord(&mut self, keys: &[Key], action: ActionFn) {
    //     let key_handler = Rc::new(RefCell::new(ChordHandler::new(keys, action)));
    //     for key in keys {
    //         let filter = KeyEventFilter {
    //             key_code: key.code(),
    //         };
    //         self.handler_map
    //             .entry(filter)
    //             .or_default()
    //             .push(key_handler.clone());
    //     }
    // }

    fn add_handler(&mut self, key_handler: impl KeyEventHandler + 'static) {
        self.handler_map
            .entry(KeyEventFilter { key_code: 0 })
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

    /// Check if the value is made up by us - and shouldn't be emitted to the virtual device
    fn is_real(&self) -> bool {
        match self.value {
            KeyEventValue::Press | KeyEventValue::Repeat | KeyEventValue::Release => true,
            _ => false,
        }
    }
}

#[derive(Eq, Hash, PartialEq, Copy, Clone)]
struct KeyEventFilter {
    key_code: u16,
}

trait KeyEventHandler: std::fmt::Debug {
    // fn handle_event(&self, proc: &mut Processor) -> Result<HandleResult>;
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, HandlerEvent)>;
    // fn reset(&mut self, pv: &mut ProcView);
    fn reset(&mut self);
    // fn dyn_eq(&self, other: &Self);
    fn get_state(&self) -> HandlerState;
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum HandlerState {
    // #[deprecated]
    // Handled,

    // Inactive states
    Waiting,

    // Active states
    BuildingUp,
    //Fulfilled, /// This is not a state
    TearingDown,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum HandlerEvent {
    Aborted,
    BuildingStarted,
    BuildComplete,
    TeardownComplete,
    NoEvent,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum KeyAction {
    // Last member has the highest prio
    PassThrough,
    Hold,
    Discard,
}

// enum HandlerResult {
//     Hold,
//     Complete,
//     Inactive
// }

fn is_active(state: HandlerState) -> bool {
    match state {
        HandlerState::BuildingUp | HandlerState::TearingDown => true,
        HandlerState::Waiting => false,
    }
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

impl<'p> Processor<'p> {
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
        let mut key_queue = VecDeque::new();
        // let mut active_handlers = Vec::new();
        // let mut inactive_handlers: Vec<HandlerRc> = Vec::new();
        let mut handlers: Vec<HandlerRc> = Vec::new();
        let active_layer = self
            .layers
            .get_mut(self.active_layer_id)
            .context("No active layer set")?;
        for v in active_layer.handler_map.values() {
            // let active = false;
            // handlers.extend(v.iter().map(|h| RefCell::new((h.to_owned(), active))));
            // let active = false;
            handlers.extend(v.iter().map(|h| h.clone()));
        }
        loop {
            let event =
                Self::get_next_event(&mut input_stream, &mut quick_repeats, &mut self.output_kb)
                    .await?;

            let active_layer = self
                .layers
                .get_mut(self.active_layer_id)
                .context("No active layer set")?;
            let mut layer_switch = false;
            let old_layer_id = self.active_layer_id;
            // let filter = KeyEventFilter {
            //     key_code: event.code(),
            // };
            let mut event_result = HandlerState::Waiting;
            let silence_unmapped = active_layer.silence_unmapped;
            let mut holders = Vec::new();
            let nice_event = event.into();
            let mut final_key_action = KeyAction::PassThrough;
            // let mut to_active = Vec::new();
            // let mut to_inactive = Vec::new();
            // Active handlers get the first opportunity. Optimize!
            ////  improve: false is lower than true, so inverting the result...
            ////             Only need to do this if any handler had an event
            handlers.sort_by_key(|h| !is_active(h.borrow().get_state()));
            // for (handler, handler_active) in active_handlers
            //     .iter()
            //     .map(|h: &HandlerRc| (h, true))
            //     .chain(inactive_handlers.iter().map(|h: &HandlerRc| (h, false)))
            for handler in &handlers {
                // if let Some(layer_handlers) = active_layer.handler_map.get_mut(&filter) {
                ////////// This should not iterate over the handlers in active_handlers!!!
                // for handler in layer_handlers
                //     .iter()e
                //     .filter(|h| !active_handlers.iter().any(|ah| Rc::ptr_eq(h, ah)))
                {
                    let old_state = handler.borrow().get_state();
                    let (key_action, handler_event) =
                        handler.borrow_mut().handle_event(&mut ProcView {
                            event: &nice_event,
                            active_layer_id: &mut self.active_layer_id,
                            output_kb: &mut self.output_kb,
                        })?;
                    let new_state = handler.borrow().get_state();
                    ensure!(
                        handler_event == HandlerEvent::NoEvent || new_state != old_state,
                        "Handler should change state when emitting handler event. {nice_event:?} -> Handler event: {handler_event:?}, for {handler:?} (old state: {old_state:?})"
                    );
                    if handler_event != HandlerEvent::NoEvent {
                        println!(
                            "{:?} {:?} {:?} {:?} ",
                            nice_event.key, nice_event.value, key_action, handler_event
                        );
                    }

                    ensure!(
                        !(handler.borrow().get_state() == HandlerState::TearingDown
                            && key_action == KeyAction::Hold),
                        "Cannot hold keys while tearing down"
                    );

                    final_key_action = std::cmp::max(key_action, final_key_action);

                    match handler_event {
                        HandlerEvent::BuildingStarted => {}
                        HandlerEvent::TeardownComplete => {}
                        HandlerEvent::Aborted => {
                            // if handler_active {
                            //     to_inactive.push(handler);
                            // }
                            // *handler_active = false;
                            Self::flush_aborted_handler(
                                &handler,
                                &mut key_queue,
                                // &mut handlers,
                                &mut self.output_kb,
                            )?;
                        }
                        HandlerEvent::BuildComplete => {
                            // if !handler_active {
                            //     to_active.push(handler);
                            // }
                            // *handler_active = true;
                            // The key made a handler complete
                            // event_result = HandlerState::Fulfilled;
                            Self::flush_completed_handler(
                                &handler,
                                &mut key_queue,
                                &handlers,
                                &mut self.output_kb,
                            )?;
                        }
                        HandlerEvent::NoEvent => (),
                    }

                    if self.active_layer_id != old_layer_id {
                        layer_switch = true;
                        break;
                    }

                    if key_action == KeyAction::Hold {
                        holders.push(handler.clone());
                    } else if key_action == KeyAction::Discard {
                        // reset anything?
                        // Key will not reach any other handler
                        break;
                    }
                }
            }

            if layer_switch {
                // New layer ID already set by callback
                // Self::reset_layer(
                //     self.layers
                //         .get_mut(old_layer_id)
                //         .context("Invalid layer id: ")?,
                //     &mut ProcView {
                //         event: &event.into(),
                //         active_layer_id: &mut self.active_layer_id,
                //         output_kb: &mut self.output_kb,
                //     },
                // );
                behåll aktiva handlers. byt ut inaktiva
                dbg!(self.active_layer_id);
            }

            if nice_event.is_real() {
                if final_key_action == KeyAction::Hold {
                    key_queue.push_back((nice_event, holders));
                }
                // if event_result == HandlerState::Waiting && !silence_unmapped && nice_event.is_real()
                else if final_key_action == KeyAction::PassThrough {
                    self.output_kb.emit(&[event])?;
                }
            }
            //// This is likely heavy
            // for handler in to_active {
            //     active_handlers.push(handler.clone());
            //     inactive_handlers.retain(|h| !Rc::ptr_eq(h, handler));
            // }
            // for handler in to_inactive {
            //     inactive_handlers.push(handler.clone());
            //     active_handlers.retain(|h| !Rc::ptr_eq(h, handler));
            // }

            if event.value() == KeyEventValue::Release as i32 {
                ///// fix
                // Clean any quick repeats
                quick_repeats.retain(|(key, _time)| key.code() != event.code());
            }

            // let have_active_handlers = handlers.iter().any(|h| is_active(h.borrow().get_state()));
            let have_building_handlers = handlers
                .iter()
                .any(|h| h.borrow().get_state() == HandlerState::BuildingUp);
            if have_building_handlers && event.value() == KeyEventValue::Press as i32 {
                quick_repeats.push_back((
                    Key::new(event.code()),
                    Instant::now() + Duration::from_millis(200),
                ));
            } else {
                quick_repeats.clear();
            }
        }
    }

    async fn get_next_event<'a>(
        input_stream: &'a mut EventStream,
        quick_repeats: &'a mut VecDeque<(Key, Instant)>,
        output_kb: &mut VirtualDevice,
    ) -> Result<InputEvent> {
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
            pin!(quick_repeat_wait);
            loop {
                let ev = select! {
                    val = input_stream.next_event() => val?,
                    val = &mut quick_repeat_wait => val,
                };
                if ev.event_type() == EventType::KEY {
                    break ev;
                } else {
                    output_kb.emit(&[ev])?;
                }
            }
        } else {
            loop {
                let ev = select! {
                    val = input_stream.next_event() => val?,
                };
                if ev.event_type() == EventType::KEY {
                    break ev;
                } else {
                    output_kb.emit(&[ev])?;
                }
            }
        };
        if event.value() == KeyEventValue::QuickRepeat as i32 {
            quick_repeats.pop_front();
        }

        return Ok(event);
    }

    fn reset_layer(layer: &mut Layer) {
        //// Reset press state from old layer? All except layer button should flush? chords can either just reset - and the keys will then send spurious releases, or they can send press+release for the captured buttons
        for handlers in layer.handler_map.values_mut() {
            for handler in handlers {
                handler.borrow_mut().reset();
            }
        }
    }

    /// Unmarks the given handler from all keys in the key queue and flushes
    /// the queue until the point of an item marked with another handler.
    fn flush_aborted_handler(
        handler: &HandlerRc,
        key_queue: &mut VecDeque<(NiceKeyInputEvent, Vec<HandlerRc>)>,
        // active_handlers: &mut Vec<HandlerRc>,
        output_kb: &mut VirtualDevice,
    ) -> Result<()> {
        // active_handlers.retain(|h| !Rc::ptr_eq(h, handler));

        let mut flush_events = Vec::new();

        let mut contiguous = true;
        for (ev, hs) in &mut *key_queue {
            hs.retain(|h| !Rc::ptr_eq(h, handler));
            if contiguous {
                if !hs.is_empty() {
                    contiguous = false;
                } else {
                    flush_events.push(ev.to_owned().into());
                }
            }
        }
        key_queue.drain(0..flush_events.len());
        println!("Flush {}", flush_events.len());
        output_kb.emit(&flush_events)?;
        Ok(())
    }

    fn flush_completed_handler(
        handler: &HandlerRc,
        key_queue: &mut VecDeque<(NiceKeyInputEvent, Vec<HandlerRc>)>,
        handlers: &Vec<HandlerRc>,
        output_kb: &mut VirtualDevice,
    ) -> Result<()> {
        // active_handlers.retain(|h| !Rc::ptr_eq(h, handler));

        // Any key used by the handler will be consumed and therefore not emitted.
        // // Any key not having `handler` must have at least one other handler, which
        // // means that it cannot be emitted.
        // key_queue.retain(|(_, hs)| {
        //     // Retain events that the handler did not claim
        //     !hs.iter().any(|h| Rc::ptr_eq(h, handler))
        // });

        // Simple model: Flushing everything that is not related to the current handler
        // and reset all other handlers
        // active_handlers
        //     .iter()
        //     .filter(|h| !Rc::ptr_eq(h, handler))
        //     .for_each(|h| h.borrow_mut().reset());
        // active_handlers.clear();
        // active_handlers.retain(|h| {
        //     if Rc::ptr_eq(h, handler) {
        //         true
        //     } else {
        //         h.borrow().reset();
        //         false
        //     }
        // });

        ///// TODO: only reset for the keys for which the handler was holding?
        /////       Resetting and flushing for unrelated handlers will not work for layer keys
        for h in handlers {
            if Rc::ptr_eq(&h, handler) {
            } else {
                h.borrow_mut().reset();
            }
        }

        // Flush events that was not held by `handler`
        let flush_events: Vec<_> = key_queue
            .into_iter()
            .filter_map(|(ev, hs)| {
                if !hs.iter().any(|h| Rc::ptr_eq(h, handler)) {
                    Some((*ev).into())
                } else {
                    None
                }
            })
            .collect();
        println!("Flush completed {}", flush_events.len());
        output_kb.emit(&flush_events)?;
        key_queue.clear();

        Ok(())
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

    home_layer.add_handler(SingleKey::new(
        Key::KEY_H,
        Box::new(|pv| {
            // println!("H {:?}", &pv.event);
            //// TODO: Emit function that filters out non-real events and also logs a warning
            if pv.event.is_real() {
                pv.output_kb
                    .emit(&[NiceKeyInputEvent::new(Key::KEY_0, pv.event.value).into()])?;
            }
            Ok(())
        }),
        None,
    ));
    home_layer.add_handler(LongPressModifier::new(
        Key::KEY_D,
        longmod::Action::Key(Key::KEY_LEFTALT),
    ));

    // home_layer.add_key_press(
    //     Key::KEY_H,
    //     Box::new(|pv| {
    //         println!("H {:?}", &pv.event);
    //         Ok(HandlerState::Fulfilled)
    //     }),
    //     None,
    // );
    home_layer.add_handler(LongPressModifier::new(
        Key::KEY_F,
        longmod::Action::Fn(Box::new(|pv| {
            if pv.event.value == KeyEventValue::QuickRepeat {
                *pv.active_layer_id = NAV_LAYER_ID;
                println!("Nav layer");
            } else if pv.event.value == KeyEventValue::Release {
                // Release happenend before layer switch -> cancel layer key
                // pv.output_kb.emit(&[
                //     NiceKeyInputEvent::new(Key::KEY_F, KeyEventValue::Press).into(),
                //     NiceKeyInputEvent::new(Key::KEY_F, KeyEventValue::Release).into(),
                // ])?;
                *pv.active_layer_id = HOME_LAYER_ID;
                println!("Home layer");
            }
            Ok(())
        })),
    ));
    // home_layer.add_handler(
    //     Key::KEY_D,
    //     LongPressModifier::new(Key::KEY_D, Key::KEY_LEFTALT).no_reset(),
    // );
    // home_layer.add_handler(
    //     Key::KEY_S,
    //     LongPressModifier::new(Key::KEY_S, Key::KEY_LEFTCTRL).no_reset(),
    // );
    // nav_layer.add_handler(
    //     Key::KEY_D,
    //     LongPressModifier::new(Key::KEY_D, Key::KEY_LEFTALT).no_reset(),
    // );
    // nav_layer.add_handler(
    //     Key::KEY_S,
    //     LongPressModifier::new(Key::KEY_S, Key::KEY_LEFTCTRL).no_reset(),
    // );
    home_layer.add_handler(ChordHandler::new(
        &[Key::KEY_U, Key::KEY_R],
        Box::new(|pv| {
            pv.output_kb.emit(
                &[
                    NiceKeyInputEvent::new(Key::KEY_SPACE, KeyEventValue::Press),
                    NiceKeyInputEvent::new(Key::KEY_SPACE, KeyEventValue::Release),
                ]
                .map(|e| e.into()),
            )?;
            Ok(())
        }),
    ));
    home_layer.add_handler(ChordHandler::new(
        &[Key::KEY_I, Key::KEY_O],
        Box::new(|pv| {
            pv.output_kb.emit(
                &[
                    NiceKeyInputEvent::new(Key::KEY_UP, KeyEventValue::Press),
                    NiceKeyInputEvent::new(Key::KEY_UP, KeyEventValue::Release),
                ]
                .map(|e| e.into()),
            )?;
            Ok(())
        }),
    ));

    // nav_layer.silence_unmapped = true;
    // nav_layer.add_key_press(
    //     Key::KEY_F,
    //     Box::new(|p| {
    //         if p.event.value == KeyEventValue::Release {
    //             *p.active_layer_id = HOME_LAYER_ID;
    //             println!("Home layer");
    //         }
    //         // Just silence any press or repeat
    //         Ok(HandlerState::Handled)
    //     }),
    //     None,
    // );
    // nav_layer.add_key_press(
    //     Key::KEY_LEFTALT,
    //     Box::new(|pv| {
    //         pv.output_kb.emit(&[(*pv.event).into()])?;
    //         Ok(HandlerState::Handled)
    //     }),
    //     None,
    // );
    nav_layer.add_handler(SingleKey::new(
        Key::KEY_I,
        Box::new(|pv| {
            pv.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_UP, pv.event.value).into()])?;
            Ok(())
        }),
        // Some(Box::new(|pv| {
        //     // This is needed to make sure the key is released when the layer is switched.
        //     // Otherwise the remapped key will be continue to be pressed.
        //     ////// only emit this if the key is currently pressed - need to save state
        //     pv.output_kb
        //         .emit(&[NiceKeyInputEvent::new(Key::KEY_UP, KeyEventValue::Release).into()])?;
        //     Ok(())
        // }
        None,
    ));
    nav_layer.add_handler(SingleKey::new(
        Key::KEY_K,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_DOWN, p.event.value).into()])?;
            Ok(())
        }),
        None,
    ));
    nav_layer.add_handler(SingleKey::new(
        Key::KEY_J,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_LEFT, p.event.value).into()])?;
            Ok(())
        }),
        None,
    ));
    nav_layer.add_handler(SingleKey::new(
        Key::KEY_L,
        Box::new(|p| {
            p.output_kb
                .emit(&[NiceKeyInputEvent::new(Key::KEY_RIGHT, p.event.value).into()])?;
            Ok(())
        }),
        None,
    ));

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
