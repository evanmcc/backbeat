use std::sync::{RwLock, Arc};
use ::synth::{Osc, Source, SoundResult};
use ::synth::SoundResult::*;

#[derive(Debug, Clone)]
pub enum Knob {
    Fixed(f64),
    LFO(Osc),
    FixedLFO {
        center: f64,
        lfo: Osc
    },
    Remote {
        remote: Arc<RwLock<f64>>,
        upper_limit: f64,
        lower_limit: f64
    }
}

use self::Knob::*;

impl Knob {
    pub fn new_remote(initial: f64, upper: f64, lower: f64) -> Knob {
        Remote {
             remote: Arc::new(RwLock::new(initial)),
             upper_limit: upper,
             lower_limit: lower
        }
    }

    pub fn val(&mut self, sample: u64) -> f64 {
        match *self {
            Remote { ref remote, .. } => {
                // need to pay attention to limits
                *remote.read().unwrap()
            },
            Fixed(v) => v,
            LFO(ref mut o) => match o.sample(sample) {
                Sample(s) => s as f64,
                _ => 0.0 as f64
            },
            FixedLFO { center, ref mut lfo } => {
                if let Sample(o) = lfo.sample(sample) {
                    center + o as f64
                } else {
                    center
                }
            }
        }
    }
}
