# keyremap

keyremap intends to emulate a programmable (mechanical) keyboard, to be a guide before commiting to a physical keyboard. It is also a way for me to learn Rust.

It works on the input device level, meaning that it receives and manipulates the raw key presses from the keyboard, before any desktop manager keyboard layout is applied. This means that it also applies to terminals outside of Wayland and X.

## Running

keyremap needs to run with root privileges to be able to grab keyboard input. In the future, it might support being run using capabilities and dropping its elevated permissions after start.

`cargo build && sudo target/debug/keyremap -k /dev/input/by-id/<keyboard>` 

To avoid locking up my inputs completely, I usually debug like this:

`sudo -v && (cargo build && sudo target/debug/keyremap --debug -k /dev/input/by-id/<keyboard> & sleep 20; sudo killall keyremap; echo)`

## Features

- Multiple layers
- Active handlers continue to be active after layer switch
- Add handler to multiple layers
- Partly overlapping handlers (e.g. for chords) supported
- Multiple handler types:
  - Single key
  - Chords
  - Long press alternate function

## Architecture

The main component is `Processor`, which receives keyboard input, processes it according to the supplied *handlers* and outputs the result.

![](processor_flow.drawio.svg)

## License

GPL 3.0+