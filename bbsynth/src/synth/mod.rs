use std::collections::VecDeque;
use std::f64::consts::PI;
use std::sync::mpsc::{channel, Receiver};
use std::sync::Arc;

mod osc;

pub use self::osc::Osc;
pub use self::osc::Waveform;
use self::osc::Waveform::*;

mod filter;
pub use self::filter::Filt;
pub use self::filter::FilterType;

mod wav;
pub use self::wav::Wav;

mod envelope;
use self::envelope::Env;

//mod effect;

const SAMPLE_RATE: f64 = 44_100.0;
const PI_2: f64 = PI * 2.0;
const FREQ_RADIANS: f64 = PI_2 / SAMPLE_RATE;
const CHANNELS: usize = 2;

// shouldn't be public
#[derive(Debug, Clone)]
pub enum SourceGraph {
    // mixer creation should make sure that the sum of the levels is 1.0
    Mixer(Vec<(SourceGraph, f32)>),
    Wave{ wav: Arc<Wav>, // since we're single threaded & don't want to copy
          started: u64 },
    Oscillator(Osc),
    Filter(Filt),
    Envelope(Env),
    Effect(Box<SourceGraph>)
}

#[derive(Debug, Clone)]
pub struct Sound {
    start_sample: u64,
    duration: u64,
    source_tree: SourceGraph
}

pub enum SoundResult {
    NotStarted,
    Finished,
    Sample(f32),
    Err(String)
}

pub struct Knob<T> {
    next: ValSource<T>,
    last: T
}

pub enum ValSource<T> {
    Chan(Receiver<T>),
    LFO(Osc)
}

use self::SourceGraph::*;
use self::SoundResult::*;

trait Source {
    fn sample(&mut self, u64) -> SoundResult;
}

impl Sound {
    pub fn new(start: u64, dur:u64, sources: SourceGraph)
           -> Sound {
        Sound {
            start_sample: start,
            duration: dur,
            source_tree: sources
        }
    }
}

impl Source for Sound {
    fn sample(&mut self, sample: u64) -> SoundResult {
        if sample >= self.start_sample + self.duration {
            return SoundResult::Finished
        }

        self.source_tree.sample(sample)
    }
}

// add range of samples for efficiency?
impl Source for SourceGraph {
    fn sample(&mut self, sample: u64) -> SoundResult {
        match *self {
            Mixer(ref mut sources) => {
                let combined =
                    sources.iter_mut().map(|& mut (ref mut src, lvl)| {
                        match src.sample(sample) {
                            Sample(s) => s * lvl,
                            _ => 0.0
                        }
                    }).sum();
                Sample(combined)
            },
            Filter(ref mut f) => {
                match f.source.sample(sample) {
                    Sample(s) => {
                        Sample(f.butterworth(s))
                    },
                    other => other
                }
            }
            Envelope(ref mut source) => source.sample(sample),
            Effect(ref mut source) => source.sample(sample),
            Wave{ ref wav, started } => {
                let pos = sample - started;
                if pos > wav.duration {
                    Finished
                } else {
                    // hack for current monophony
                    Sample(wav.samples[(pos*2) as usize])
                }
            }
            Oscillator(ref mut osc) => {
                match osc.waveform {
                    Sine => {
                        let phase_inc = FREQ_RADIANS * osc.frequency;
                        let samp = osc.phase.sin() as f32;
                        osc.phase += phase_inc;
                        if osc.phase >= PI_2 {
                            osc.phase -= PI_2
                        }
                        // println!("sample {} {} {} ", sample, samp, osc.phase);
                        Sample(samp)
                    },
                    Saw => {
                        let phase_inc = FREQ_RADIANS * osc.frequency;
                        let samp = ((osc.phase / PI) - 1.0) as f32;
                        osc.phase += phase_inc;
                        if osc.phase >= PI_2 {
                            osc.phase -= PI_2
                        }
                        Sample(samp)
                    }
                    Tri => {
                        let phase_inc = FREQ_RADIANS * osc.frequency;
                        let mut tri_imdt = (osc.phase * (2.0 / PI)) as f32;
                        if osc.phase < 0.0 {
                            tri_imdt += 1.0
                        } else {
                            tri_imdt = 1.0 - tri_imdt
                        }
                        osc.phase += phase_inc;
                        if osc.phase >= PI {
                            osc.phase -= PI_2
                        }
                        Sample(tri_imdt)
                    }
                    Square => {
                        let sample_time = 1.0 / SAMPLE_RATE;
                        let period = 1.0 / osc.frequency;
                        let midpoint = period * 0.5;
                        let mut samp: f32;
                        if osc.phase < midpoint {
                            samp = 1.0
                        } else {
                            samp = -1.0
                        }
                        osc.phase += sample_time;
                        if osc.phase >= period {
                            osc.phase -= period
                        }
                        Sample(samp)
                    }
                }
            }
        }
    }
}

pub struct Synth {
    pub current_sample: u64,
    // sorted by sample start, so we can break iteration when we hit a
    // sample start that's in the future.
    sounds: VecDeque<Sound>,
}

impl Synth {
    pub fn new() -> Synth {
        Synth {
            current_sample: 0,
            sounds: VecDeque::new()
        }
    }

    pub fn add_sound(& mut self, sound: Sound) {
        //println!("adding sound");
        self.sounds.push_back(sound)
    }

    pub fn sample(& mut self) -> (f32, f32) {
        let mut samp_acc: f32 = 0.0;
        let mut remove = false;
        for (i, sound) in self.sounds.iter_mut().enumerate() {
            if sound.start_sample > self.current_sample {
                break
            }
            //println!("samp {} {:?}", self.current_sample, sound);
            match sound.sample(self.current_sample) {
                Sample(s) => samp_acc += s,
                NotStarted => continue,
                Finished => remove = true,
                Err(string) => println!("sample error: {} {}",
                                        self.current_sample, string)
            }
        }
        if remove {
            let sample = self.current_sample;
            self.sounds.retain(
                |ref item|
                (item.start_sample + item.duration) > sample
            )
        }
        self.current_sample += 1;
        (samp_acc, samp_acc)
    }
}
