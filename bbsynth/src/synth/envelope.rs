

use synth::SourceGraph;
use synth::Source;
use synth::SoundResult;
use synth::SoundResult::*;

#[derive(Debug, Clone)]
pub struct Env {
    source: Box<SourceGraph>,
    segments: Vec<Segment>,
    sample: u64
}

#[derive(Debug, Clone)]
struct Segment {
    curve: Curve,
    target: f64,
    duration: u64,
    relase: bool
}

#[derive(Debug, Clone)]
enum Curve {
    Linear,
    Exp(f64),
    Log(f64)
}

impl Source for Env {
    fn sample(&mut self, sample: u64) -> SoundResult {
        match self.source.sample(sample) {
            Sample(in_sample) => {
                let seg = get_segment(&self.segments, sample, self.sample);
                Sample(in_sample)
            }
            other => return other
        }
    }
}

fn get_segment(segments: &Vec<Segment>, current: u64,
               start: u64) -> Segment {
    let mut position = current - start;
    for seg in segments.iter() {
        if position < seg.duration {
            position -= seg.duration;
            continue;
        } else {
            return seg.clone()
        }
    }
    // this feels wrong?
    segments.last().unwrap().clone()
}

