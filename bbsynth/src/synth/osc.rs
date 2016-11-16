pub trait Source {
    fn sample(&Self, time: u64) -> f32;
}

pub enum Waveform {
    Sine,
    Saw,
    Tri,
    Square
}

use Waveform::*;

pub struct Osc {
    waveform: Waveform,
    frequency: f32,
    phase: f64
}

pub fn new_osc(w: Waveform, f: f32) -> Osc {
    Osc {
        waveform: w,
        frequency: f,
        phase: 0.0
    }
}

impl Source for Osc {
    fn sample(&self, time) {
        match self.waveform {
            Sine => {
                0.0
            },
            Saw => {
                let phase_increment = frequency_radians * self.frequency;
                let sample = (self.phase / PI) - 1;
                self.phase = (self.phase += phase_increment) % PI_2;
                sample
            }
            _ => 0.0
        }
    }

}
