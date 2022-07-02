// entrypoint for the bb_nif module

#![feature(plugin)]
#![feature(custom_attribute)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
extern crate portaudio;

use rustler::{ atom, NifEnv, NifTerm, NifResult,
               NifEncoder, NifError };
use rustler::list::NifListIterator;
use rustler::atom::{NifAtom, get_atom};
use rustler::binary::NifBinary;
use rustler::resource::ResourceCell;
use rustler::map::{get_map_value};
use rustler::tuple::{get_tuple, make_tuple};

use portaudio as pa;

use std::mem;
use std::thread;
use std::sync::mpsc::{channel, Sender};
use std::time;
use std::collections::HashMap;
use std::sync::{RwLock, Arc};

mod synth;

use synth::{Synth, Osc, Wav, Filt, Knob, Sound};
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
    sender: Sender<SynthMsg>,
    knobs: HashMap<String, Arc<RwLock<f64>>>
}

rustler_export_nifs!(
    "bbsynth",
    [
        ("init_resources", 0, init_resources),
        ("play_note", 5, play_note),
        ("terminate", 1, terminate),
        ("load_wav", 5, load_wav),
        ("create_instrument", 3, create_instrument),
        ("twiddle", 3, twiddle)
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

fn twiddle<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let res: ResourceCell<Resource> = args[0].decode()?;
    let name: String = args[1].decode()?;
    let new_val: f64 = args[2].decode()?;
    let ref knobs = res.read().unwrap().knobs;
    let knob = knobs.get(&name).unwrap();
    let mut k = knob.write().unwrap();
    let old_val = *k;
    *k = new_val;
    Ok(old_val.encode(env))
}

fn create_instrument<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let res: ResourceCell<Resource> = args[0].decode()?;
    let sender = res.read().unwrap().sender.clone();

    let name: String = args[1].decode()?;
    let inst: NifTerm = args[2].decode()?;
    match instrument(env, inst) {
        Ok((iknobs, graph)) => {
            let i = Instrument {
                source: graph,
                name: name
            };

            // add the instrument internally
            sender.send(i);
            // save the knobs and gather them up for return to erlang
            let mut ret: Vec<NifTerm> = Vec::new();
            let ref mut knobs = res.write().unwrap().knobs;
            for knob in iknobs {
                if let Knob::Remote { name, remote, .. } = knob {
                    knobs.insert(name.clone(), remote.clone());
                    ret.push(name.encode(env));
                }
            }
            Ok(ret.encode(env))
        },
        Err(err) => Err(err)
    }
}

//#[NifTuple] struct MixerArgs<'a> { source: NifTerm<'a>, level: f64 }

fn instrument<'a>(env: &'a NifEnv, map: NifTerm) -> NifResult<(Vec<Knob>, SourceGraph)> {
    let at_type: NifTerm = atom::get_atom_init("type").to_term(env);
    let at_ftype: NifTerm = atom::get_atom_init("ftype").to_term(env);
    let at_sources: NifTerm = atom::get_atom_init("sources").to_term(env);
    let at_source: NifTerm = atom::get_atom_init("source").to_term(env);
    let at_value: NifTerm = atom::get_atom_init("value").to_term(env);
    let at_waveform: NifTerm = atom::get_atom_init("waveform").to_term(env);
    let at_knob: NifTerm = atom::get_atom_init("knob").to_term(env);
    let at_fknob: NifTerm = atom::get_atom_init("fknob").to_term(env);

    fn parse_knob(env: &NifEnv, kterm: NifTerm)
                  -> Result<(Vec<Knob>, Knob), NifError> {
        let mut kvec = get_tuple(kterm).ok().unwrap();
        let mut knobs: Vec<Knob> = Vec::new();
        let ktype: String = kvec[0].decode()?;
        let knob: Knob =
            match &*ktype {
                "fixed" => {
                    Knob::Fixed(kvec[1].decode()?)
                },
                "lfo" => {
                    let (mut rec_knobs, source) = instrument(env, kvec[1]).ok().unwrap();
                        if let Oscillator(osc) = source {
                            knobs.append(&mut rec_knobs);
                            Knob::LFO(osc)
                        } else { panic!("no lfo") }
                },
                "fixed_lfo" => {
                    let (mut rec_knobs, source) = instrument(env, kvec[1]).ok().unwrap();
                    if let Oscillator(osc) = source {
                        knobs.append(&mut rec_knobs);
                        Knob::FixedLFO{ center: kvec[2].decode()?, lfo: osc }
                    } else { panic!("no lfo") }
                },
                "remote" => {//  n, u, l
                    Knob::Remote{ name: kvec[1].decode()?,
                                  upper_limit: kvec[2].decode()?,
                                  lower_limit: kvec[3].decode()?,
                                  remote: Arc::new(RwLock::new(0.0))
                    }
                },
                // make sure this doesn't crash everything?
                _ => panic!("bad knob")
            };
        Ok((knobs, knob))
    }

    match get_map_value(map, at_type).unwrap().decode()? {
        "mixer" => {
            let inputs_term = get_map_value(map, at_sources).unwrap();
            let inputs: NifListIterator = inputs_term.decode()?;
            let mut knobs: Vec<Knob> = Vec::new();
            let mut sources: Vec<(SourceGraph, f32)> = Vec::new();
            let mut level_total: f32 = 0.0;
            for input in inputs {
                if let Ok(i) = get_tuple(input) {
                    let level: f32 = i[1].decode()?;
                    let (mut rec_knobs, source) = instrument(env, i[0]).ok().unwrap();
                    level_total += level;
                    sources.push((source, level));
                    knobs.append(&mut rec_knobs);
                }
            }
            if 0.98 <= level_total && level_total <= 1.02 {
                Ok((knobs, Mixer(sources)))
            } else {
                Err(NifError::Atom("bad_mixer_total"))
            }
        },
        "osc" => {
            let waveform = match get_map_value(map, at_waveform).unwrap().decode()? {
                "sine" => Sine,
                "saw" => Saw,
                "square" => Square,
                "noise" => Noise,
                "triangle" => Tri,
                _ => panic!("bad waveform")
            };
            let kterm: NifTerm = get_map_value(map, at_knob).unwrap();
            let (remotes, knob) = parse_knob(env, kterm)?;
            Ok((remotes, Oscillator(
                Osc { waveform: waveform,
                      frequency: Arc::new(knob),
                      lfo: None,
                      phase: 0.0 })))
        },
        "filter" => {
            let s = get_map_value(map, at_source).unwrap();
            let (mut knobs, source) = instrument(env, s).ok().unwrap();

            let k = get_map_value(map, at_fknob).unwrap();
            let (mut knobs2, knob) = parse_knob(env, k)?;

            let ftype: String = get_map_value(map, at_ftype).unwrap().decode()?;
            let f =
                match &*ftype {
                    "lowpass" => Lowpass(knob),
                    "highpass" => Highpass(knob),
                    "bandpass" => Bandpass(knob),
                    _ => panic!("unknown filter type")
                };
            let filt = Filt::new(source, f);
            knobs.append(&mut knobs2);
            Ok((knobs, Filter(filt)))
        },
        "env" => Err(NifError::Atom("noes")),
        "wav" => Err(NifError::Atom("noes")),
        "effect" => Err(NifError::Atom("noes")),
        "knob" => Err(NifError::Atom("noes")),
        _ => Err(NifError::Atom("unknown_source_type"))
    }
}

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
    let mut res = ResourceCell::new(Resource {
        sender: tx,
        knobs: HashMap::new()
    });

    let knob = Knob::new_remote("default".to_string(), 2000.0, 22100.0, 0.0);
    if let Knob::Remote { remote, ..} = knob.clone() {
        res.write().unwrap().knobs.insert("default".to_string(), remote.clone());
    }

    let mut synth = Synth::new();
    let mut instruments: HashMap<String, SourceGraph> = HashMap::new();
    let mut samples: HashMap<String, Arc<Wav>> = HashMap::new();


    let callback =
        move|pa::OutputStreamCallbackArgs { buffer, frames, .. }| {

            // get any messages that might be waiting, but not too many
            let mut limit = 100;
            let mut exit = false;

            let default_instrument =
                Mixer(vec![
                    (Filter(
                        Filt::new(
                            Oscillator(
                                Osc {
                                    waveform: Saw,
                                    lfo: None,
                                    frequency: Arc::new(Knob::Trigger),
                                    phase: 0.0
                                }),
                            Lowpass(knob.clone()))),
                     0.95),
                    (Oscillator(
                        Osc {
                            waveform: Noise,
                            lfo: None,
                            frequency: Arc::new(Knob::Fixed(200.0)),
                            phase: 0.0
                        }),
                     0.05)]);

            while let Ok(msg) = rx.try_recv() {
                match msg {
                    Note { ref instrument, start, duration, pitch } => {
                        // make this default instrument live in instruments
                        let sound: Sound =
                            if let Some(inst) = instruments.get(instrument) {
                                Sound::new(start, duration, pitch as f64, inst.clone())
                            } else if let Some(samp) = samples.get(instrument) {
                                Sound::new(start, duration, pitch as f64,
                                           Wave { started: synth.current_sample,
                                                  wav: samp.clone() })
                            } else {
                                Sound::new(start, duration, pitch as f64,
                                           default_instrument.clone())
                            };
                        synth.add_sound(sound)
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
