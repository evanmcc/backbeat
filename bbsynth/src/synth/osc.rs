
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

