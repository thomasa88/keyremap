use anyhow::Result;
use evdev::{
    uinput::{VirtualDevice, VirtualDeviceBuilder},
    Device, EventType, InputEvent, Key,
};

// type Action = dyn Fn(&mut ProcState, &mut VirtualDevice);
type Action = dyn Fn(ProcView);
type BoxAction = Box<Action>;

fn ba<'v, 'p: 'v, T: Fn(ProcView<'v, 'p>)>(action: T) -> Box<T> {
    Box::new(action)
}

#[derive(Debug, PartialEq)]
enum ProcessResult {
    Consumed,
    NotConsumed,
}

struct Layer<'a> {
    handlers: &'a [&'a dyn KeyHandler],
    // chords: &'a [Chord],
}

trait KeyHandler {
    fn process_event(&self, event: &InputEvent, view: ProcView) -> ProcessResult;
}

struct KeyPress {
    key: Key,
    action: BoxAction,
}

impl KeyPress {
    fn new(key: Key, action: BoxAction) -> Self {
        Self {
            key,
            action: action,
        }
    }
}

impl KeyHandler for KeyPress {
    fn process_event(&self, event: &InputEvent, view: ProcView) -> ProcessResult {
        if event.code() == self.key.code() {
            (self.action)(view);
            ProcessResult::Consumed
        } else {
            ProcessResult::NotConsumed
        }
    }
}

enum KH {
    Key(KeyPress),
    UpKey(KeyPress),
    Chord(Chord),
}

struct Chord {
    action: BoxAction,
}

struct Processor<'a> {
    devices: EvdevDevices,
    // key_presses: &'a [KeyPress],
    state: ProcState<'a>,
}

struct ProcView<'v, 'p> {
    state: &'v mut ProcState<'p>,
    virt_kb: &'v mut VirtualDevice,
}

struct EvdevDevices {
    input_kb: Device,
    virt_kb: VirtualDevice,
}

#[derive(Default)]
struct ProcState<'a> {
    active_layer: Option<&'a Layer<'a>>,
}

impl<'a> Processor<'a> {
    // fn set_active_layer(&mut self, layer: &'a Layer) {
    //     self.state.active_layer = Some(layer);
    // }

    fn run(&mut self) -> Result<()> {
        ///// use get_key_state to figure out the initial state and release any pressed key:
        //self.input_kb.get_key_state()

        for event in self.devices.input_kb.fetch_events()? {
            if event.event_type() == EventType::KEY {
                Self::process_key_event(&mut self.state, &mut self.devices.virt_kb, event)?;
            } else {
                self.devices.virt_kb.emit(&[event])?;
            }
        }
        Ok(())
    }

    fn process_key_event(
        state: &mut ProcState,
        virt_kb: &mut VirtualDevice,
        event: InputEvent,
    ) -> Result<ProcessResult> {
        if let Some(layer) = state.active_layer {
            for kp in layer.handlers {
                if kp.process_event(&event, ProcView { state, virt_kb }) == ProcessResult::Consumed
                {
                    break;
                    // (kp.action)(ProcView { state, virt_kb });
                }
            }
            // for kp in layer.key_presses {
            //     (kp.action)(ProcView { state, virt_kb });
            // }
        }
        virt_kb.emit(&[event])?;
        Ok(ProcessResult::Consumed)
    }
}

fn main() -> Result<()> {
    // let space_chord = {}

    const NAV_LAYER: Layer = Layer { handlers: &[] };

    // let num_key = KeyPress {
    //     key: Key::KEY_F,
    //     action: Box::new(|state| {
    //         state.active_layer = Some(&NAV_LAYER);
    //     }),
    // };
    let num_key = KeyPress::new(
        Key::KEY_F,
        ba(&|pv: ProcView| pv.state.active_layer = Some(&NAV_LAYER)),
    );
    let test_key = KeyPress::new(
        Key::KEY_G,
        // Box::new(&|state: &mut ProcState, virt_kb: &mut VirtualDevice| {
        Box::new(&|pv: ProcView| {
            pv.virt_kb.emit(&[]).ok();
        }),
    );
    let first_layer = Layer {
        handlers: &[&test_key, &num_key],
    };

    let mut virt_kb = VirtualDeviceBuilder::new()?
        .name("keyremap")
        // .with_keys(&keys)?
        .build()?;

    let mut unguarded_dev =
        Device::open("/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-kbd")?;
    let mut p = Processor {
        devices: EvdevDevices {
            input_kb: unguarded_dev,
            virt_kb,
        },
        state: ProcState {
            ..Default::default()
        },
    };
    p.state.active_layer = Some(&NAV_LAYER);
    p.run();
    // match p.process() {
    //     ProcessResult::KeyPressed(kp) => ((kp.action)(&mut p.state)),
    //     ProcessResult::None => (),
    // }
    Ok(())
}
