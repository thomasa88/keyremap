use std::{thread::sleep, time::Duration};

use anyhow::{Context, Result};
use evdev::{uinput::VirtualDeviceBuilder, EventType, InputEvent, InputEventKind, Key};
use num_traits::FromPrimitive;
use scopeguard::guard;
use tracing::info;

#[derive(num_derive::FromPrimitive, Debug)]
enum KeyAction {
    Release = 0,
    Press = 1,
    Repeat = 2,
}



fn main() -> Result<()> {
    tracing_subscriber::fmt().init();

    let mut unguarded_dev =
        evdev::Device::open("/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-kbd")?;
    let mut dev = guard(&mut unguarded_dev, |d| {
        d.ungrab().unwrap();

        // Releasing keys on the virt_kb is likely not needed,
        // as the device is removed when exiting.
    });

    // let mut keys = AttributeSet::<Key>::new();
    // keys.insert(Key::KEY_SPACE);
    let keys = dev.supported_keys().context("Failed to get keys")?;

    let mut virt_kb = VirtualDeviceBuilder::new()?
        .name("keyremap")
        .with_keys(&keys)?
        .build()?;

    for path in virt_kb.enumerate_dev_nodes_blocking()? {
        println!("Virtual keyboard path: {}", path?.display());
    }

    // Wait for any pressed key (used to start the command) to be released before grabbing...
    // TODO: Make sure to release all keys.. (loop all of them? :O) Is it possible to check which keys are down?
    // ioctl(EVIOCGKEY) can read the current state of a key.
    sleep(Duration::from_millis(1000));
    dev.grab()?;

    #[derive(PartialEq, Debug)]
    enum S {
        None,
        First,
        Second,
    }
    let mut state = S::None;
    loop {
        for ev in dev.fetch_events()? {
            if let InputEventKind::Key(code) = ev.kind() {
                let mut matched = false;
                let action = ev.value();
                if code == Key::KEY_J {
                    state = match state {
                        S::None if action == KeyAction::Press as i32 => S::First,
                        S::First | S::Second if action == KeyAction::Release as i32 => S::None,
                        _ => state,
                    };
                    matched = true;
                } else if code == Key::KEY_K {
                    state = match state {
                        S::First if action == KeyAction::Press as i32 => S::Second,
                        S::First | S::Second if action == KeyAction::Release as i32 => S::None,
                        _ => state,
                    };
                    matched = true;
                }
                info!(?state);
                if state == S::Second {
                    let down = InputEvent::new(EventType::KEY, Key::KEY_SPACE.code(), KeyAction::Press as i32);
                    let up = InputEvent::new(EventType::KEY, Key::KEY_SPACE.code(), KeyAction::Release as i32);
                    virt_kb.emit(&[down, up])?;
                    state = S::None;
                }
                info!(?state);
                if matched {
                    // Event processed
                    continue;
                }
            }
            // match ev.kind() {
            //     InputEventKind::Key(code) => {
            //         let _event: KeyAction = FromPrimitive::from_i32(ev.value())
            //             .context(format!("Bad event {}", ev.value()))?;
            //         // println!("{code:?} {event:?}")

            //         match code {
            //             Key::KEY_J => {
            //                 info!("Press");
            //                 // KeyAction::Press as i32
            //                 let event =
            //                     InputEvent::new(EventType::KEY, Key::KEY_SPACE.code(), ev.value());
            //                 virt_kb.emit(&[event])?;
            //             }
            //             _ => {
            //                 // let event = InputEvent::new(EventType::KEY, Key::KEY_SPACE.code(), ev.value());
            //                 virt_kb.emit(&[ev])?;
            //             }
            //         }
            //     }

            // Just forward all unprocessed events
            virt_kb.emit(&[ev])?;
        }
    }
}
