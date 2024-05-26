mod chord;
mod longmod;
mod single_key;

use std::any::{type_name, type_name_of_val};
use std::cmp::Ordering;
use std::collections::BTreeSet;
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
use itertools::Itertools;
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
type HandlerBox = Box<RefCell<dyn KeyEventHandler>>;

struct Layer {
    id: usize,
    // handlers: Vec<TaggedHandler>,
    silence_unmapped: bool,
}

impl Layer {
    fn new(id: usize) -> Self {
        Self {
            id,
            // handlers: vec![],
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

    // fn add_handler(&mut self, key_handler: HandlerRc) {
    //     self.handlers.push(TaggedHandler {
    //         handler: key_handler,
    //         layers: self.id,
    //     });
    // }
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

trait KeyEventHandler: Debug + Display {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, HandlerEvent)>;
    fn reset(&mut self);
    fn get_state(&self) -> HandlerState;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
enum HandlerState {
    // First member has the highest prio
    // It is more important that a handler completes tearing down
    // than a handler to start building up, so we don't get
    // overlapped handling. Same for BuildUp vs Waiting.
    TearingDown,
    BuildingUp,
    Waiting,
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
    // First member has the highest prio
    Discard,
    Hold,
    PassThrough,
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

struct Processor {
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
    all_handlers: Vec<TaggedHandler>,
}

struct ProcView<'v> {
    // Provide full event, to allow for multiple keys handled by same handler as well as timeout handling
    event: &'v NiceKeyInputEvent,
    active_layer_id: &'v mut usize,
    output_kb: &'v mut VirtualDevice,
}

#[derive(Debug)]
struct TaggedHandler {
    handler: HandlerBox,
    // By having a list of layers inside each handler, we allow the to belong to multiple layers
    // and can easily filter the total set of handlers for the handlers beloning to a specific
    // layer. An alternative is to let the Layer type contain a list of handlers and don't store
    // the layer ids with the handler. However, this makes it more complicated when switching layers,
    // as the code will need to search the active layer for every handler that has completed tearing
    // down, to see if it belongs to another layer and should be removed from the set of active handlers.
    // Initially, this was accomplished together with providing one (1) layer ID for each handler,
    // but that limited each handler (instance) to belong to only one (1) layer.
    layers: Box<[usize]>,
}

impl Display for TaggedHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {}", &self.layers, self.handler.borrow())
    }
}

impl PartialEq for TaggedHandler {
    fn eq(&self, other: &Self) -> bool {
        self.layers == other.layers
            && std::ptr::addr_eq(self.handler.as_ptr(), other.handler.as_ptr())
        //Rc::ptr_eq(&self.handler, &other.handler)
    }
}

impl PartialOrd for TaggedHandler {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for TaggedHandler {}

impl Ord for TaggedHandler {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.layers.cmp(&other.layers) {
            Ordering::Equal => self.handler.as_ptr().cmp(&other.handler.as_ptr()), //Rc::as_ptr(&self.handler).cmp(&Rc::as_ptr(&other.handler)),
            cmp_res => cmp_res,
        }
    }
}

impl Processor {
    fn new(input_kb: Device, output_kb: VirtualDevice, layers: Vec<Layer>) -> Self {
        assert!(layers.len() > 0);
        let first_layer_id = layers[0].id;
        Self {
            input_kb,
            output_kb,
            layers: layers,
            active_layer_id: first_layer_id,
            all_handlers: Vec::new(),
        }
    }

    // fn add_handler_box(&mut self, for_layers: &[usize], key_handler: HandlerBox) {
    //     self.all_handlers.push(TaggedHandler {
    //         handler: key_handler,
    //         layers: for_layers.into(),
    //     })
    // }

    fn add_handler(&mut self, for_layers: &[usize], key_handler: impl KeyEventHandler + 'static) {
        self.all_handlers.push(TaggedHandler {
            handler: Box::new(RefCell::new(key_handler)),
            layers: for_layers.into(),
        })
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
        ////////// todo: possible to work with references instead of clones inside run()? all layer members should keep existing (at least if layers are not mutable)
        let mut key_queue: VecDeque<(NiceKeyInputEvent, Vec<&TaggedHandler>)> = VecDeque::new();
        let mut handlers;

        {
            // let mut active_layer = self
            //     .layers
            //     .get_mut(self.active_layer_id)
            //     .context("No active layer set")?;

            //let handlers = &mut active_layer.handlers;
            handlers = self
                .all_handlers
                .iter()
                .filter(|th| th.layers.contains(&self.active_layer_id))
                .collect_vec();
        }
        loop {
            let event =
                Self::get_next_event(&mut input_stream, &mut quick_repeats, &mut self.output_kb)
                    .await?;

            let mut holders = Vec::new();
            let nice_event = event.into();
            let mut final_key_action = KeyAction::PassThrough;

            let mut event_happened = false;
            let mut have_building_handlers = false;
            let mut new_layer_id = self.active_layer_id;
            for handler in &*handlers {
                let old_state = handler.handler.borrow().get_state();
                let (key_action, handler_event) =
                    handler.handler.borrow_mut().handle_event(&mut ProcView {
                        event: &nice_event,
                        active_layer_id: &mut new_layer_id,
                        output_kb: &mut self.output_kb,
                    })?;
                let new_state = handler.handler.borrow().get_state();

                if new_state == HandlerState::BuildingUp {
                    have_building_handlers = true;
                }

                if handler_event != HandlerEvent::NoEvent || key_action != KeyAction::PassThrough {
                    event_happened = true;

                    println!(
                        "{:?} -> {:?} {:?} {:?} -> {:?}",
                        nice_event,
                        handler.layers,
                        handler.handler.borrow(),
                        handler_event,
                        key_action,
                    );
                }

                ensure!(
                        handler_event == HandlerEvent::NoEvent || new_state != old_state,
                        "Handler should change state when emitting handler event. {nice_event:?} -> Handler event: {handler_event:?}, for {:?} (old state: {old_state:?})", &handler.handler
                    );
                ensure!(
                    !(handler.handler.borrow().get_state() == HandlerState::TearingDown
                        && key_action == KeyAction::Hold),
                    "Cannot hold keys while tearing down"
                );

                final_key_action = std::cmp::min(key_action, final_key_action);

                match handler_event {
                    HandlerEvent::BuildingStarted => {}
                    HandlerEvent::TeardownComplete => {}
                    HandlerEvent::Aborted => {
                        Self::flush_aborted_handler(
                            &handler,
                            &mut key_queue,
                            // &mut handlers,
                            &mut self.output_kb,
                        )?;
                    }
                    HandlerEvent::BuildComplete => {
                        // The key made a handler complete
                        Self::flush_completed_handler(
                            &handler,
                            &mut key_queue,
                            &mut self.output_kb,
                        )?;
                    }
                    HandlerEvent::NoEvent => (),
                }

                if new_layer_id != self.active_layer_id {
                    break;
                }

                if key_action == KeyAction::Hold {
                    holders.push(*handler); //handler.clone());
                } else if key_action == KeyAction::Discard {
                    // reset anything?
                    // Key will not reach any other handler
                    break;
                }
            }

            if event_happened {
                if new_layer_id != self.active_layer_id {
                    // New layer ID already set by callback

                    // Abort all handlers that are building up - they did not finish "in time"
                    for handler in handlers
                        .iter()
                        .filter(|th| th.handler.borrow().get_state() == HandlerState::BuildingUp)
                    {
                        Self::flush_aborted_handler(handler, &mut key_queue, &mut self.output_kb)?;
                    }

                    // Add the handlers of the "new" layer
                    for new_handler in self
                        .all_handlers
                        .iter()
                        .filter(|th| th.layers.contains(&self.active_layer_id))
                    {
                        if !handlers.contains(&new_handler) {
                            handlers.push(new_handler);
                        }
                    }

                    dbg!(self.active_layer_id);
                    dbg!(&handlers);
                }

                // Remove handlers that don't belong to the active layer - if they are not (still) tearing down
                handlers.retain(|th| {
                    th.handler.borrow().get_state() == HandlerState::TearingDown
                        || th.layers.contains(&self.active_layer_id)
                });

                // Order handlers by state.
                // Active handlers get the first opportunity. Optimize!?
                handlers.sort_unstable_by_key(|h| h.handler.borrow().get_state());
            }

            if nice_event.is_real() {
                // Must push to key queue if the queue is not empty to avoid emitting a key before any held keys
                if final_key_action == KeyAction::Hold || !key_queue.is_empty() {
                    key_queue.push_back((nice_event, holders));
                }
                // if event_result == HandlerState::Waiting && !silence_unmapped && nice_event.is_real()
                else if final_key_action == KeyAction::PassThrough {
                    println!("EMIT: {nice_event:?}");
                    self.output_kb.emit(&[event])?;
                }
            }

            if have_building_handlers {
                if nice_event.value == KeyEventValue::Release {
                    // Clean any quick repeats for the key
                    quick_repeats.retain(|(key, _time)| key.code() != event.code());
                } else if nice_event.value == KeyEventValue::Press {
                    quick_repeats.push_back((
                        Key::new(event.code()),
                        Instant::now() + Duration::from_millis(200),
                    ));
                }
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

    /// Unmarks the given handler from all keys in the key queue and flushes
    /// the queue until the point of an item marked with another handler.
    fn flush_aborted_handler(
        handler: &TaggedHandler,
        key_queue: &mut VecDeque<(NiceKeyInputEvent, Vec<&TaggedHandler>)>,
        // active_handlers: &mut Vec<HandlerRc>,
        output_kb: &mut VirtualDevice,
    ) -> Result<()> {
        // active_handlers.retain(|h| !Rc::ptr_eq(h, handler));

        let mut flush_events = Vec::new();

        let mut contiguous = true;
        for (ev, hs) in &mut *key_queue {
            hs.retain(|th| th != &handler);
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
        handler: &TaggedHandler,
        key_queue: &mut VecDeque<(NiceKeyInputEvent, Vec<&TaggedHandler>)>,
        output_kb: &mut VirtualDevice,
    ) -> Result<()> {
        let mut overlapping_handlers = BTreeSet::new();

        // Discard all held keys that are held by the handler and discover
        // any handlers that overlapped the handler.
        key_queue.retain(|(_ev, hs)| {
            if hs.contains(&handler) {
                for th in hs {
                    if th == &handler {
                        continue;
                    }
                    overlapping_handlers.insert(*th);
                }
                false
            } else {
                true
            }
        });

        // Abort all overlapping handlers and make sure that their held keys are flushed out
        for th in overlapping_handlers {
            th.handler.borrow_mut().reset();
            Self::flush_aborted_handler(th, key_queue, output_kb)?;
        }

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

    let layers = vec![home_layer, nav_layer];
    let mut proc = Processor::new(input_kb, virt_kb, layers);

    let h_to_0 = SingleKey::new(
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
    );
    proc.add_handler(&[HOME_LAYER_ID, NAV_LAYER_ID], h_to_0);
    // proc.add_handler(&[HOME_LAYER_ID], &LongPressModifier::new(
    //     Key::KEY_D,
    //     longmod::Action::Key(Key::KEY_LEFTALT),
    // ));

    proc.add_handler(
        &[HOME_LAYER_ID],
        LongPressModifier::new(
            Key::KEY_F,
            longmod::Action::Fn(Box::new(|pv| {
                if pv.event.value == KeyEventValue::QuickRepeat {
                    *pv.active_layer_id = NAV_LAYER_ID;
                    println!("Nav layer");
                } else if pv.event.value == KeyEventValue::Release {
                    *pv.active_layer_id = HOME_LAYER_ID;
                    println!("Home layer");
                }
                Ok(())
            })),
        ),
    );
    // // proc.add_handler(&[HOME_LAYER_ID],
    // //     Key::KEY_D,
    // //     LongPressModifier::new(Key::KEY_D, Key::KEY_LEFTALT).no_reset(),
    // // );
    // // proc.add_handler(&[HOME_LAYER_ID],
    // //     Key::KEY_S,
    // //     LongPressModifier::new(Key::KEY_S, Key::KEY_LEFTCTRL).no_reset(),
    // // );
    // // proc.add_handler(&[NAV_LAYER_ID],
    // //     Key::KEY_D,
    // //     LongPressModifier::new(Key::KEY_D, Key::KEY_LEFTALT).no_reset(),
    // // );
    // // proc.add_handler(&[NAV_LAYER_ID],
    // //     Key::KEY_S,
    // //     LongPressModifier::new(Key::KEY_S, Key::KEY_LEFTCTRL).no_reset(),
    // // );
    proc.add_handler(
        &[HOME_LAYER_ID],
        ChordHandler::new(
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
        ),
    );
    proc.add_handler(
        &[HOME_LAYER_ID],
        ChordHandler::new(
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
        ),
    );

    // // nav_layer.silence_unmapped = true;
    proc.add_handler(
        &[NAV_LAYER_ID],
        SingleKey::new(
            Key::KEY_I,
            Box::new(|pv| {
                pv.output_kb
                    .emit(&[NiceKeyInputEvent::new(Key::KEY_UP, pv.event.value).into()])?;
                Ok(())
            }),
            None,
        ),
    );
    proc.add_handler(
        &[NAV_LAYER_ID],
        SingleKey::new(
            Key::KEY_K,
            Box::new(|p| {
                p.output_kb
                    .emit(&[NiceKeyInputEvent::new(Key::KEY_DOWN, p.event.value).into()])?;
                Ok(())
            }),
            None,
        ),
    );
    proc.add_handler(
        &[NAV_LAYER_ID],
        SingleKey::new(
            Key::KEY_J,
            Box::new(|p| {
                p.output_kb
                    .emit(&[NiceKeyInputEvent::new(Key::KEY_LEFT, p.event.value).into()])?;
                Ok(())
            }),
            None,
        ),
    );
    proc.add_handler(
        &[NAV_LAYER_ID],
        SingleKey::new(
            Key::KEY_L,
            Box::new(|p| {
                p.output_kb
                    .emit(&[NiceKeyInputEvent::new(Key::KEY_RIGHT, p.event.value).into()])?;
                Ok(())
            }),
            None,
        ),
    );

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
