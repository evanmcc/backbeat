use std::collections::VecDeque;
use std::f64::consts::PI;

const SAMPLE_RATE: f64 = 44_100.0;
const PI_2: f64 = PI * 2.0;
const FREQ_RADIANS: f64 = PI_2 / SAMPLE_RATE;

// shouldn't be public
#[derive(Debug)]
pub enum SourceGraph {
    // mixer creation should make sure that the sum of the levels is 1.0
    Mixer(Vec<(SourceGraph, f32)>),
    Osc {
        waveform: Waveform,
        frequency: f64,
        phase: f64
    },
    Wav {},
    Filter(Box<SourceGraph>),
    Envelope(Box<SourceGraph>),
    Effect(Box<SourceGraph>)
}

#[derive(Debug)]
pub enum Waveform {
    Sine,
    Saw,
    Tri,
    Square
}

use self::Waveform::*;

#[derive(Debug)]
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
            Filter(ref mut source) => source.sample(sample),
            Envelope(ref mut source) => source.sample(sample),
            Effect(ref mut source) => source.sample(sample),
            Wav {..} => Err("not yet implemented".to_string()),
            Osc { ref waveform, frequency, ref mut phase } => {
                match *waveform {
                    Sine => {
                        let phase_inc = FREQ_RADIANS * frequency;
                        let samp = phase.sin() as f32;
                        *phase += phase_inc;
                        if *phase >= PI_2 {
                            *phase -= PI_2
                        }
                        // println!("sample {} {} {} ", sample, samp, phase);
                        Sample(samp)
                    },
                    _ => Sample(0.0)
                }
            }
        }
    }
}

pub struct Synth {
    current_sample: u64,
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
