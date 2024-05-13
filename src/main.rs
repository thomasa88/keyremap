use std::{cell::OnceCell, marker::PhantomData, sync::Once, vec};

use anyhow::Result;
use evdev::{
    uinput::{VirtualDevice, VirtualDeviceBuilder},
    Device, Key,
};
use tracing_subscriber::layer;

type ActionFn = Box<dyn Fn(&mut Processor) -> Result<()>>;

struct Layer {
    id: usize,
    handlers: Vec<Box<dyn KeyEventHandler>>,
}

impl Layer {
    // fn new() -> Layer {
    //     Layer {
    //     }
    // }

    fn add_key_press(&mut self, key: Key, action: ActionFn) {
        self.handlers.push(Box::new(KeyPress { key, action }));
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
            handlers: vec![],
        }
    }
}

struct KeyPress {
    key: Key,
    action: ActionFn,
}

impl KeyEventHandler for KeyPress {
    fn handle_event(&self, proc: &mut Processor) -> Result<HandleResult> {
        Ok(HandleResult::NotHandled)
    }
}

trait KeyEventHandler {
    fn handle_event(&self, proc: &mut Processor) -> Result<HandleResult>;
}

enum HandleResult {
    Handled,
    NotHandled,
}

struct Processor<'a> {
    input_kb: Device,
    output_kb: VirtualDevice,
    //////// Not technically needed to store layers in Processor
    layers: Vec<Layer>,
    // Using a usize, as we cannot store a reference to the layer
    // Possible alternative: Rc<Box<Layer>>
    active_layer_id: usize,
    // active_layer: Option<&'a mut Layer>,
    _phantom_data: std::marker::PhantomData<&'a ()>, // temp, while refactoring
}

struct ProcView<'v> {
    active_layer: &'v usize,
    output_kb: &'v VirtualDevice,
}

impl<'a> ProcView<'a> {
    fn set_active_layer(layer: &mut Layer) {}
}

impl<'a> Processor<'a> {
    fn new(input_kb: Device, output_kb: VirtualDevice, layers: Vec<Layer>) -> Self {
        assert!(layers.len() > 0);
        let first_layer_id = layers[0].id;
        let p = Self {
            input_kb,
            output_kb,
            layers: layers,
            active_layer_id: first_layer_id,
            _phantom_data: PhantomData,
        };
        // p.active_layer = Some(p.layers.get_mut(0).unwrap());
        p
    }

    fn run(&mut self) {
        //// cannot pass self to action, as we will already have a mut borrow?
        // self.active_layer = Some(&mut self.layers[0]);
        //self.set_active_layer(&self.layers[0]);
        Self::reset_layer(&mut self.layers[self.active_layer_id]);
        self.active_layer_id = 0;
        for event in self.input_kb.fetch_events() {}
        // let s = self.layers[0].handlers[0].as_mut().handle_event(&mut *self);
        // self.active_layer = Some(&mut self.layers[0]);
    }

    // fn set_active_layer(&mut self, new_layer: &Layer) {
    fn set_active_layer(&mut self, new_layer_id: usize) -> Result<()> {
        // Reset the loaded layer instead?
        let old_layer_id = self.active_layer_id;
        Self::reset_layer(&mut self.layers.get_mut(old_layer_id).unwrap());
        self.active_layer_id = new_layer_id;
        Ok(())
    }
    
    fn reset_layer(layer: &mut Layer) {
        //// go through layer and reset handlers/state
        //// Reset press state from old layer? All except layer button should flush? chords can either just reset - and the keys will then send spurious releases, or they can send press+release for the captured buttons
    }
}

fn main() -> Result<()> {
    let mut virt_kb = VirtualDeviceBuilder::new()?
        .name("keyremap")
        // .with_keys(&keys)?
        .build()?;
    let mut unguarded_dev =
        Device::open("/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-kbd")?;

    let mut lf = LayerFactory::new();
    const FIRST_LAYER_ID: usize = 0;
    let mut first_layer = lf.create_layer();

    first_layer.add_key_press(
        Key::KEY_F,
        Box::new(|p| {
            // pv.active_layer = first_layer.id;
            Processor::set_active_layer(p, FIRST_LAYER_ID)?;
            Ok(())
        }),
    );
    let id = first_layer.id;

    let layers = vec![first_layer];
    let mut proc = Processor::new(unguarded_dev, virt_kb, layers);

    // let l = proc.layers.get(id).unwrap();
    // proc.set_active_layer(&l);

    Ok(())
}
