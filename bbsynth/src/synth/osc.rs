use std::f64::consts::PI;
use std::sync::Arc;
use synth::rand::thread_rng;
use synth::rand::distributions::Range;
use synth::rand::distributions::IndependentSample;

use ::synth::{SAMPLE_RATE, FREQ_RADIANS, PI_2};
use ::synth::{Source, SoundResult, Knob};
use ::synth::SoundResult::*;
use ::synth::Knob::*;

#[derive(Debug, Clone)]
pub struct Osc {
    pub waveform: Waveform,
    pub frequency: Arc<Knob>,
    pub lfo: Option<Arc<Osc>>,
    pub phase: f64
}

/// because FM setups are so complicated, I think that I'll try
/// parsing them as simple strings.  A FM descriptor (simple, we can't
/// target envelopes and filters at individual operators, that's
/// something that needs more thinking) would look like
/// "NWWRR:RR.RRR:AAA|I,RR.RRR:AAA|O;" N is the 1-indexed operator
/// number. WW is a two letter reference to the waveform of the
/// operator: sa, sq, si, etc. RR.RRR is the ratio for an operator.
/// For the primary it will be ignored unless the primary loops back
/// (sets I to itself).  AAA is the amplitude of the oscillator for
/// that direction.  I and O are indexes in the chain. THE CHAIN IS
/// 1-INDEXED Chains longer than 8 operators should be rejected.  9 is
/// the output, 0 is the null input.  I may not reference nonexistent
/// operators.  At least one output must go to 9.  I is optional, for
/// use with back-chaining.

/// NB: for ease of parsing these strings are fixed width.  all
/// numbers must be 0 padded if they aren't long enough.


// #[derive(Debug, Clone)]
// pub struct FMOsc {
//     operators: Vec<Operator>
// }

// pub struct Operator {
//     waveform: Waveform,
//     frequency: OpFreq,
//     input: u8,
//     target: u8,
// }

// pub enum OpFreq {
//     Primary(Knob), // must be a trigger?

#[derive(Debug, Clone)]
pub enum Waveform {
    Sine,
    Saw,
    Tri,
    Square,
    Noise,
    //PinkNoise
}
use self::Waveform::*;

impl Source for Osc {
    fn sample(&mut self, sample: u64) -> SoundResult {
        let ref mut frequency = Arc::get_mut(&mut self.frequency).unwrap();
        match self.waveform {
            Sine => {
                let phase_inc = FREQ_RADIANS * (*frequency).val(sample);
                let samp = self.phase.sin() as f32;
                self.phase += phase_inc;
                if self.phase >= PI_2 {
                    self.phase -= PI_2
                }
                // println!("sample {} {} {} ", sample, samp, self.phase);
                Sample(samp)
            },
            Saw => {
                let phase_inc = FREQ_RADIANS * frequency.val(sample);
                let samp = ((self.phase / PI) - 1.0) as f32;
                self.phase += phase_inc;
                if self.phase >= PI_2 {
                    self.phase -= PI_2
                }
                Sample(samp)
            },
            Tri => {
                let phase_inc = FREQ_RADIANS * frequency.val(sample);
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
            },
            Square => {
                let sample_time = 1.0 / SAMPLE_RATE;
                let period = 1.0 / frequency.val(sample);
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
            },
            Noise => {
                let between = Range::new(-1.0, 1.0);
                let mut rng = thread_rng();
                Sample(between.ind_sample(&mut rng))
            }
        }
    }
}

// impl Source for FMOsc {
//     fn sample(&mut self, sample: u64) -> SoundResult {
//         let ratio = self.ratio.val();
//         let ret = carrier.sample(sample);
        

//         Sample(ret)
//     }
// }

impl Osc {
    pub fn triggers(&self) -> u64 {
        let main =
            match *self.frequency {
                Trigger => 1,
                _ => 0
            };
        if let Some(ref lfo) = self.lfo {
            main + (*lfo).triggers()
        } else {
            main
        }
    }

    pub fn make_rec(&self, pitch: f64) -> Osc{
        let mut ret: Osc
            = match *self.frequency {
                Trigger => Osc{
                    frequency: Arc::new(Fixed(pitch)),
                    .. self.clone()
                },
                _ => self.clone()
            };
        if let Some(_) = self.lfo {
            ret.lfo = Some(Arc::new(self.make_rec(pitch)))
        }
        ret
    }
}
