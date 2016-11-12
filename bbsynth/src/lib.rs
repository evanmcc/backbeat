// entrypoint for the bb_nif module

#![feature(plugin)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
extern crate portaudio;
#[macro_use]
extern crate lazy_static;

use rustler::{ atom, NifEnv, NifTerm, NifResult, NifEncoder };
use portaudio as pa;
use std::collections::VecDeque;
use std::sync::Mutex;
use std::thread;

const SAMPLE_RATE: f64 = 44_100.0;
const CHANNELS: i32 = 2;
const FRAMES: u32 = 256;
const INTERLEAVED: bool = true;

lazy_static! {
    static ref CHANNEL: Mutex<VecDeque<SynthMsg>> =
        Mutex::new(VecDeque::new());

}

rustler_export_nifs!(
    "bb_nif",
    [
        ("play_note", 3, play_note),
        ("terminate", 0, terminate)
    ],
    Some(on_load)
);

enum SynthMsg {
    Note {
        duration: usize, // in samples
        pitch: usize, // in Hz
    },
    Exit
}

use SynthMsg::*;

fn play_note<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    Ok((1).encode(env))
}

fn terminate<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let mut queue = CHANNEL.lock().unwrap();
    queue.push_front(Exit);
    Ok(atom::get_atom_init("ok").to_term(env))
}

fn on_load(env: &NifEnv, load_info: NifTerm) -> bool {
    // println!("{:#?}", load_info.decode());

    thread::spawn(
        move|| {

            let pa = pa::PortAudio::new().unwrap();

            println!("PortAudio init");
            println!("\tversion: {}", pa.version());
            println!("\tversion text: {:?}", pa.version_text());
            println!("\thost count: {}", pa.host_api_count().unwrap());

            let host = pa.default_host_api().unwrap();
            println!("\thost: {:#?}\n", pa.host_api_info(host));

            let output = pa.default_output_device().unwrap();
            let output_info = pa.device_info(output).unwrap();
            println!("Default output device info: {:#?}", &output_info);

            // Construct the output stream parameters.
            let latency = output_info.default_low_output_latency;
            let params = pa::StreamParameters::<f32>::new(output, CHANNELS, INTERLEAVED, latency);

            // Check that the stream format is supported.
            pa.is_output_format_supported(params, SAMPLE_RATE).unwrap();

            let settings = pa::OutputStreamSettings::new(params, SAMPLE_RATE, FRAMES);
            let mut current_sample = 0;

            let mut stream = pa.open_blocking_stream(settings).unwrap();
            stream.start().unwrap();
            let mut queue = CHANNEL.lock().unwrap();

            'main: loop {
                // get our available frames
                let frames = handle_stream(|| stream.write_available());

                // get any messages that might be waiting, but not too many
                let mut limit = 100;
                while let Some(msg) = queue.pop_back() {
                    match msg {
                        Note {..} => {},
                        Exit => break 'main
                    }

                    limit -= 1;
                    if limit == 0 {
                        break;
                    }
                }
                // write samples
                for _ in 0..frames {
                    
                    current_sample += 1;
                }
            }
        });
    true
}

fn handle_stream<F>(f: F) -> usize
    where F: Fn() -> Result<pa::StreamAvailable, pa::error::Error>
{
    match f() {
        Ok(avail) => {
            match avail {
                pa::StreamAvailable::Frames(frames) => frames as usize,
                //shouldn't be possible?
                pa::StreamAvailable::InputOverflowed => 0,
                pa::StreamAvailable::OutputUnderflowed => {
                    println!("underflow");
                    0
                }
            }
        },
        Err(error) => panic!("output stream error: {}", error)
    }
}
