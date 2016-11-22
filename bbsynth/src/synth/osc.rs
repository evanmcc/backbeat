use std::f64::consts::PI;
use ::synth::{SAMPLE_RATE, FREQ_RADIANS, PI_2};
use ::synth::{Source, SoundResult};
use ::synth::SoundResult::*;

#[derive(Debug, Clone)]
pub struct Osc {
    pub waveform: Waveform,
    pub frequency: f64,
    pub phase: f64
}

#[derive(Debug, Clone)]
pub enum Waveform {
    Sine,
    Saw,
    Tri,
    Square
}
use self::Waveform::*;

impl Source for Osc {
    fn sample(&mut self, sample: u64) -> SoundResult {
        match self.waveform {
            Sine => {
                let phase_inc = FREQ_RADIANS * self.frequency;
                let samp = self.phase.sin() as f32;
                self.phase += phase_inc;
                if self.phase >= PI_2 {
                    self.phase -= PI_2
                }
                // println!("sample {} {} {} ", sample, samp, self.phase);
                Sample(samp)
            },
            Saw => {
                let phase_inc = FREQ_RADIANS * self.frequency;
                let samp = ((self.phase / PI) - 1.0) as f32;
                self.phase += phase_inc;
                if self.phase >= PI_2 {
                    self.phase -= PI_2
                }
                Sample(samp)
            }
            Tri => {
                let phase_inc = FREQ_RADIANS * self.frequency;
                let mut tri_imdt = (self.phase * (2.0 / PI)) as f32;
                if self.phase < 0.0 {
                    tri_imdt += 1.0
                } else {
                    tri_imdt = 1.0 - tri_imdt
                }
                self.phase += phase_inc;
                if self.phase >= PI {
                    self.phase -= PI_2
                }
                Sample(tri_imdt)
            }
            Square => {
                let sample_time = 1.0 / SAMPLE_RATE;
                let period = 1.0 / self.frequency;
                let midpoint = period * 0.5;
                let mut samp: f32;
                if self.phase < midpoint {
                    samp = 1.0
                } else {
                    samp = -1.0
                }
                self.phase += sample_time;
                if self.phase >= period {
                    self.phase -= period
                }
                Sample(samp)
            }
        }
    }
}
