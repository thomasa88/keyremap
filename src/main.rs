mod chord;
mod longmod;
mod single_key;

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::Debug;
use std::{cell::RefCell, collections::VecDeque, fmt::Display, time::Duration};

use anyhow::{ensure, Context, Result};
use chord::ChordHandler;
use clap::Parser;
use evdev::{Device, EventStream, EventType, InputEvent, Key};
use itertools::Itertools;
use longmod::LongPressModifier;
use num_traits::FromPrimitive;
use scopeguard::guard;
use single_key::SingleKey;
use tokio::{pin, select, time::Instant};
use tracing::{debug, info};
use Iterator;

#[cfg(not(test))]
use evdev::uinput::{VirtualDevice, VirtualDeviceBuilder};

/// Delay for the synthetic [`KeyEventValue::QuickRepeat`] to occur after [`KeyEventValue::Press`]
const QUICK_REPEAT_DELAY: Duration = Duration::from_millis(200);

type ActionFn = Box<dyn FnMut(&mut ProcView) -> Result<()>>;
type ResetFn = Box<dyn FnMut()>;
type HandlerBox = Box<RefCell<dyn KeyEventHandler>>;

#[derive(clap::Parser, Debug)]
struct Args {
    /// Enables debug prints
    #[arg(long)]
    debug: bool,

    /// Keyboard evdev input device to use. Can be found in /dev/input/by-id.
    #[arg(short, long, value_name = "INPUT_DEV")]
    keyboard: String,
}

struct LayerConfig {
    id: usize,
    silence_unmapped: bool,
}

/// Enum mapping of [`evdev::InputEvent::value()`]
#[derive(num_derive::FromPrimitive, Debug, PartialEq, Clone, Copy, Eq, Hash)]
enum KeyEventValue {
    Release = 0,
    Press = 1,
    Repeat = 2,
    /// Injected synthetic event, that fires more quickly than [`Self::Repeat`]. It is only fired once per [`Self::Press`].
    /// It is used for reacting to "long presses", but more quickly than [`Self::Repeat`].
    QuickRepeat = 99,
}

/// A partial copy of [`evdev::InputEvent`], with members exposed as enums instead of integers.
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

/// Receives keyboard input events and matches them to a set of internal conditions, such
/// as a specific key or group of keys being pressed.
///
/// structs of this trait make up the main configuration of the application.
trait KeyEventHandler: Debug + Display {
    fn handle_event(&mut self, pv: &mut ProcView) -> Result<(KeyAction, HandlerEvent)>;
    fn reset(&mut self);
    fn get_state(&self) -> HandlerState;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
/// Processing state of a [`KeyEventHandler`]
///
/// ![](../../../handler_state.drawio.svg)
///
/// See also [`HandlerEvent`].
///
/// First member has the highest priority.
/// It is more important that a handler completes tearing down
/// than a handler to start building up, so we don't get
/// overlapped handling. Same for `BuildingUp` vs `Waiting`.
enum HandlerState {
    /// Handler requirements have been fulfilled and the handler is cleaning up.
    /// Typically for key release events, complementary to the press events to fulfill the handler's conditions,
    /// that should not be propagated.
    TearingDown,
    /// Handler is still collecting key events, as key events matching the handler has been received,
    /// but its condition has not been fulfilled.
    /// E.g., a chord waiting for another key.
    BuildingUp,
    /// Waiting to receive key events that match the handler
    Waiting,
}

/// Indicates a [`KeyEventHandler`] [state](`HandlerState`) transition.
///
/// Transition information is needed as a handler could transition to a new state
/// for different reasons and [`Processor`] acts differently based on the handler
/// event that happened. A transition to [`HandlerState::Waiting`] could
/// occur both due to [`Self::Aborted`] and [`Self::TeardownComplete`] events.
///
/// ![](../../../handler_state.drawio.svg)
///
/// See also [`HandlerState`].
#[derive(Debug, PartialEq, Clone, Copy)]
enum HandlerEvent {
    /// Handler state is reset and any keys held in the key queue by the handler are released.
    Aborted,
    /// Handler has started matching on the incoming key events.
    BuildingStarted,
    /// The handler condition is fulfilled and it has fired its action.
    BuildComplete,
    /// The handler has finished tearing down.
    TeardownComplete,
    /// The handler has not changed its state.
    NoEvent,
}

/// Action for [`Processor`] to perform for the given key input event. Emitted by [`KeyEventHandler`].
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum KeyAction {
    // First member has the highest prio
    Discard,
    Hold,
    PassThrough,
}

/// Processes input keyboard events, passes them through handlers and sends resulting output events.
/// Keeps track of what key events are "held" by the handlers, blocking them from being emitted
/// until they have been released (all handlers aborted) or discarding them (a handler has completed
/// its condition).
/// 
/// ![](../../../processor_flow.drawio.svg)
struct Processor {
    input_kb: Device,
    output_kb: VirtualDevice,
    layers: Vec<LayerConfig>,
    /// ID of the currently active layer.
    /// Using a `usize`, as it is not possible store a reference to the layer,
    /// because would reference to another member of the same struct.
    active_layer_id: usize,
    all_handlers: Vec<TaggedHandler>,
}

/// Provides access a key event and [`Processor`] members to key event handlers, without hitting borrow limits.
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
            Ordering::Equal => self.handler.as_ptr().cmp(&other.handler.as_ptr()),
            cmp_res => cmp_res,
        }
    }
}

impl Processor {
    fn new(input_kb: Device, output_kb: VirtualDevice, layers: Vec<LayerConfig>) -> Self {
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

            let mut handler_event_happened = false;
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

                if handler_event != HandlerEvent::NoEvent {
                    //|| key_action != KeyAction::PassThrough {
                    handler_event_happened = true;

                    debug!(
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
                    holders.push(*handler);
                } else if key_action == KeyAction::Discard {
                    // reset anything?
                    // Key will not reach any other handler
                    break;
                }
            }

            if handler_event_happened {
                if new_layer_id != self.active_layer_id {
                    self.active_layer_id = new_layer_id;

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

                    debug!("Active layer: {}", self.active_layer_id);
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
                } else if final_key_action == KeyAction::PassThrough
                    && !self
                        .layers
                        .get(self.active_layer_id)
                        .context("Get silence unmapped from active layer")?
                        .silence_unmapped
                {
                    self.output_kb.emit(&[event])?;
                }
            }

            if have_building_handlers {
                if nice_event.value == KeyEventValue::Release {
                    // Clean any quick repeats for the key
                    quick_repeats.retain(|(key, _time)| key.code() != event.code());
                } else if nice_event.value == KeyEventValue::Press {
                    quick_repeats
                        .push_back((Key::new(event.code()), Instant::now() + QUICK_REPEAT_DELAY));
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
        // futures::FuturesUnordered wants the Future<> type to be the same for all pushes.
        // tokio::JoinSet::spawn() tries to move the borrowed input_stream

        async fn quick_repeat_wait(key: &Key, time: &Instant) -> InputEvent {
            tokio::time::sleep_until(*time).await;
            NiceKeyInputEvent {
                key: *key,
                value: KeyEventValue::QuickRepeat,
            }
            .into()
        }

        let event = if let Some((key, time)) = quick_repeats.front() {
            let quick_repeat_wait = quick_repeat_wait(key, time);
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
                let ev = input_stream.next_event().await?;
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
        debug!("Flush {}", flush_events.len());
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

// Tokio spawn() requires arguments to be Sync, even though it is only used on
// one thread. Using tokio::task::LocalSet instead - to still have sleep.
// Another alternative is async_executor::LocalExecutor (but can we sleep using it?)
#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    let args = Args::parse();

    let log_level = if args.debug {
        tracing::level_filters::LevelFilter::DEBUG
    } else {
        tracing::level_filters::LevelFilter::INFO
    };
    tracing::subscriber::set_global_default(
        tracing_subscriber::fmt()
            .with_max_level(log_level)
            .with_timer(tracing_subscriber::fmt::time::LocalTime::rfc_3339())
            .finish(),
    )?;

    let input_kb = Device::open(&args.keyboard)
        .with_context(|| format!("Open keyboard \"{}\"", &args.keyboard))?;

    let supported_keys = input_kb.supported_keys().context("Failed to get keys")?;
    let virt_kb = VirtualDeviceBuilder::new()?
        .name("keyremap")
        .with_keys(&supported_keys)?
        .build()?;

    // TODO: Figure out better setup for layers. Right now, the code expect the ids to be the position in
    // the Vec. That is OK, but then the ID should be assigned automatically.
    const HOME_LAYER_ID: usize = 0;
    const NAV_LAYER_ID: usize = 1;
    let layer_configs = Vec::from([
        LayerConfig {
            id: HOME_LAYER_ID,
            silence_unmapped: false,
        },
        LayerConfig {
            id: NAV_LAYER_ID,
            silence_unmapped: true,
        },
    ]);

    let mut proc = Processor::new(input_kb, virt_kb, layer_configs);

    // let h_to_0 = SingleKey::new(
    //     Key::KEY_H,
    //     Box::new(|pv| {
    //         // println!("H {:?}", &pv.event);
    //         //// TODO: Emit function that filters out non-real events and also logs a warning
    //         if pv.event.is_real() {
    //             pv.output_kb
    //                 .emit(&[NiceKeyInputEvent::new(Key::KEY_0, pv.event.value).into()])?;
    //         }
    //         Ok(())
    //     }),
    //     None,
    // );
    // proc.add_handler(&[HOME_LAYER_ID, NAV_LAYER_ID], h_to_0);

    proc.add_handler(
        &[HOME_LAYER_ID],
        LongPressModifier::new(
            Key::KEY_F,
            longmod::Action::Fn(Box::new(|pv| {
                if pv.event.value == KeyEventValue::QuickRepeat {
                    *pv.active_layer_id = NAV_LAYER_ID;
                    info!("Nav layer");
                } else if pv.event.value == KeyEventValue::Release {
                    *pv.active_layer_id = HOME_LAYER_ID;
                    info!("Home layer");
                }
                Ok(())
            })),
        ),
    );
    proc.add_handler(
        &[HOME_LAYER_ID, NAV_LAYER_ID],
        LongPressModifier::new(Key::KEY_D, longmod::Action::Key(Key::KEY_LEFTALT)),
    );
    proc.add_handler(
        &[HOME_LAYER_ID, NAV_LAYER_ID],
        LongPressModifier::new(Key::KEY_S, longmod::Action::Key(Key::KEY_LEFTCTRL)),
    );
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
