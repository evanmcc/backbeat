
#[derive(Debug, Clone)]
pub struct Wav {
    pub channels: u64,
    pub duration: u64,
    pub samples: Vec<f32>
}

impl Wav {
    pub fn new(channels: usize, duration: usize, samples: Vec<i16>) -> Wav {
        let mut w = Wav {
            samples: vec![0.0; channels * duration],
            channels: channels as u64,
            duration: duration as u64
        };
        for (index, s) in samples.iter().enumerate() {
            let fs = *s as f32 / 32767 as f32;
            w.samples[index] = fs;
        }
        w
    }
}
