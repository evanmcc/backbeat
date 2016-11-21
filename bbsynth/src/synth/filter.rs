


use ::synth::SAMPLE_RATE;
use ::synth::PI_2;
use ::synth::SourceGraph;

use std::f64::consts::PI;

const root_2: f64 = 1.414213562; //(2.0 as f64).sqrt();

#[derive(Debug, Clone)]
pub struct Filt {
    pub source: Box<SourceGraph>,
    pub ftype: FilterType,
    pub delay_in1: f64,
    pub delay_in2: f64,
    pub delay_out1: f64,
    pub delay_out2: f64
}

#[derive(Debug, Clone)]
pub enum FilterType {
    Highpass(f64),
    Lowpass(f64),
    Bandpass(f64),
    //BandReject(f64)
}
use self::FilterType::*;

impl Filt {
    pub fn butterworth(& mut self, in_sample: f32) -> f32 {
        let amp_in0: f64;
        let amp_in1: f64;
        let amp_in2: f64;
        let amp_out1: f64;
        let amp_out2: f64;
        match self.ftype {
            Lowpass(cutoff) => {
                let c = 1.0 / (cutoff * (PI / SAMPLE_RATE)).tan();
                let c_sqr = c * c;
                let croot = c * root_2;
                let d = c_sqr + croot + 1.0;
                amp_in0 = 1.0 / d;
                amp_in1 = amp_in0 * 2.0;
                amp_in2 = amp_in0;
                amp_out1 = (2.0 * (1.0 - c_sqr)) / d;
                amp_out2 = (c_sqr - croot + 1.0) / d;
            },
            Highpass(cutoff) => {
                let c = 1.0 / (cutoff * (PI / SAMPLE_RATE)).tan();
                let c_sqr = c * c;
                let croot = c * root_2;
                let d = c_sqr + croot + 1.0;
                amp_in0 = 1.0 / d;
                amp_in1 = -(amp_in0 * 2.0);
                amp_in2 = amp_in0;
                amp_out1 = (2.0 * (c_sqr) - 1.0) / d;
                amp_out2 = (c_sqr - croot + 1.0) / d;
            },
            Bandpass(cutoff) => {
                let c = 1.0 / (cutoff * (PI / SAMPLE_RATE)).tan();
                let d = 1.0 + c;
                amp_in0 = 1.0 / d;
                amp_in1 = 0.0;
                amp_in2 = (c - 1.0) / d;
                amp_out1 = (-c * 2.0 * (PI_2 * (cutoff / SAMPLE_RATE).cos())) / d;
                amp_out2 = (c - 1.0) / d;
            }
        }
        let out = (amp_in0 * in_sample as f64)
            + (amp_in1 * self.delay_in1)
            + (amp_in2 * self.delay_in2)
            - (amp_out1 * self.delay_out1)
            - (amp_out2 * self.delay_out2);
        self.delay_out2 = self.delay_out1;
        self.delay_out1 = out;
        self.delay_in2 = self.delay_in1;
        self.delay_in1 = in_sample as f64;
        out as f32
    }
}
