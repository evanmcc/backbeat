// entrypoint for the bb_nif module

#![feature(plugin)]
#![feature(custom_attribute)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
extern crate portaudio;

use rustler::{ atom, NifEnv, NifTerm, NifResult, NifEncoder };
use rustler::binary::NifBinary;
use rustler::resource::ResourceCell;
use portaudio as pa;

use std::mem;
use std::thread;
use std::sync::mpsc::{channel, Sender};
use std::time;
use std::collections::HashMap;
use std::sync::Arc;

mod synth;

use synth::{Synth, Osc, Wav, Filt};
//abstract this away? please?
use synth::SourceGraph;
use synth::SourceGraph::*;
use synth::Waveform::*;
use synth::FilterType::*;

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
        ("terminate", 1, terminate),
        ("load_wav", 5, load_wav)
    ],
    Some(on_load)
);

enum SynthMsg {
    Note {
        instrument: String,
        start: u64, // sample position
        duration: u64, // in samples
        pitch: f32 // in Hz
    },
    Instrument {
        name: String,
        source: SourceGraph
    },
    Sample {
        name: String,
        data: Wav
    },
    Exit
}

use SynthMsg::*;

fn play_note<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let res: ResourceCell<Resource> = args[0].decode()?;
    let send = res.read().unwrap().sender.clone();

    let instrument: String = args[1].decode()?;
    let start: u64 = args[2].decode()?;
    let pitch: f32 = args[3].decode()?;
    let duration: u64 = args[4].decode()?;
    send.send(Note { instrument: instrument,
                     start: start,
                     pitch: pitch,
                     duration: duration });
    Ok(atom::get_atom_init("ok").to_term(env))
}

fn terminate<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let res: ResourceCell<Resource> = args[0].decode()?;
    let send = res.read().unwrap().sender.clone();
    send.send(Exit);
    Ok(atom::get_atom_init("ok").to_term(env))
}

fn load_wav<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    unsafe {
        let mut res: ResourceCell<Resource> = args[0].decode()?;
        let name: String = args[1].decode()?;
        let channels: u64 = args[2].decode()?;
        let duration: u64 = args[3].decode()?;
        let data1: NifBinary = args[4].decode()?;
        let data0 = data1.as_slice();
        let mut data = vec![0; data0.len() / 2];
        for (idx, val)  in data.iter_mut().enumerate() {
            let idx0 = idx * 2;
            *val = mem::transmute::<[u8; 2], i16>([data0[idx0], data0[idx0+1]]);
        }
        let wav = Wav::new(channels as usize, duration as usize, data);
        let send = res.read().unwrap().sender.clone();

        send.send(Sample { name: name,
                           data: wav });
    }
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
    let mut instruments: HashMap<String, SourceGraph> = HashMap::new();
    let mut samples: HashMap<String, Arc<Wav>> = HashMap::new();

    let callback =
        move|pa::OutputStreamCallbackArgs { buffer, frames, .. }| {

            // get any messages that might be waiting, but not too many
            let mut limit = 100;
            let mut exit = false;
            while let Ok(msg) = rx.try_recv() {
                match msg {
                    Note { ref instrument, start, duration, pitch } => {
                        let sound =
                            if let Some(inst) = instruments.get(instrument) {
                                (*inst).clone()
                            } else if let Some(samp) = samples.get(instrument) {
                                Wave { started: synth.current_sample,
                                       wav: samp.clone() }
                            } else {
                                Filter( Filt {
                                    source: Box::new(Oscillator(
                                    Osc {
                                        waveform: Saw,
                                        frequency: pitch as f64,
                                        phase: 0.0
                                    })),
                                    ftype: Highpass(100.0),
                                    delay_in1: 0.0,
                                    delay_in2: 0.0,
                                    delay_out1: 0.0,
                                    delay_out2: 0.0
                                })
                            };
                        synth.add_sound(synth::Sound::new(start, duration, sound))
                    },
                    Instrument { .. } => continue,
                    Sample { name, data } => {
                        samples.insert(name, Arc::new(data));
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
