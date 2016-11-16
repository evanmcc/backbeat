// entrypoint for the bb_nif module

#![feature(plugin)]
#![feature(custom_attribute)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
extern crate portaudio;

use rustler::{ atom, NifEnv, NifTerm, NifResult, NifEncoder };
use rustler::resource::ResourceCell;
use portaudio as pa;
use std::thread;
use std::sync::mpsc::{channel, Sender};
use std::time;

mod synth;

use synth::{Synth};
//abstract this away? please?
use synth::SourceGraph::*;
use synth::Waveform::*;

const SAMPLE_HZ: f64 = 44_100.0;
const CHANNELS: i32 = 2;
const FRAMES: u32 = 128;

#[NifResource]
struct Resource {
    sender: Sender<SynthMsg>
}

rustler_export_nifs!(
    "bbsynth",
    [
        ("init_resources", 0, init_resources),
        ("play_note", 5, play_note),
        ("terminate", 1, terminate)
    ],
    Some(on_load)
);

enum SynthMsg {
    Note {
        // instrument
        start: u64, // sample position
        duration: u64, // in samples
        pitch: f32 // in Hz
    },
    Exit
}

use SynthMsg::*;

fn play_note<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let res: ResourceCell<Resource> = args[0].decode()?;
    let send = res.read().unwrap().sender.clone();

    // ignore instrument for now
    let start: u64 = args[2].decode()?;
    let pitch: f32 = args[3].decode()?;
    let duration: u64 = args[4].decode()?;
    send.send(Note { start: start, pitch: pitch, duration: duration });
    Ok(atom::get_atom_init("ok").to_term(env))
}

fn terminate<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let res: ResourceCell<Resource> = args[0].decode()?;
    let send = res.read().unwrap().sender.clone();
    send.send(Exit);
    Ok(atom::get_atom_init("ok").to_term(env))
}

fn on_load(env: &NifEnv, load_info: NifTerm) -> bool {
    resource_struct_init!(Resource, env);
    true
}

fn init_resources<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let (tx, rx) = channel::<SynthMsg>();
    let res = ResourceCell::new(Resource {
        sender: tx
    });


    let mut synth = Synth::new();

    let callback =
        move|pa::OutputStreamCallbackArgs { buffer, frames, .. }| {

            // get any messages that might be waiting, but not too many
            let mut limit = 100;
            let mut exit = false;
            while let Ok(msg) = rx.try_recv() {
                match msg {
                    Note { start, duration, pitch } => {
                        let sound: synth::Sound =
                            synth::Sound::new(start, duration,
                                              Osc{ waveform: Sine,
                                                   frequency: pitch as f64,
                                                   phase: 0.0
                                              });
                        synth.add_sound(sound)
                    },
                    Exit => exit = true
                }

                limit -= 1;
                if limit == 0 {
                    break;
                }
            }
            // write samples
            for i in 0..frames {
                let idx = i * 2;
                let (l, r) = synth.sample();
                buffer[idx] = l;
                buffer[idx+1] = r;
            }
            if !exit {
                pa::Continue
            } else {
                pa::Complete
            }
        };
    thread::spawn(move || {
        let pa = pa::PortAudio::new().unwrap();
        let settings = pa.default_output_stream_settings::<f32>(CHANNELS,
                                                                SAMPLE_HZ,
                                                                FRAMES).unwrap();
        let mut stream = pa.open_non_blocking_stream(settings, callback).unwrap();
        stream.start().unwrap();

        let ten_ms = time::Duration::from_millis(10);

        while let Ok(true) = stream.is_active() {thread::sleep(ten_ms);}
    });

    let ret = res.encode(env);
    Ok(ret)
}
