struct Delay {
    taps: Vec<Tap>,
    line: Vec<f32>,
    delay: usize
}

impl Delay {
    fn new(size: usize, channels: usize) -> Delay {
        Delay {
            taps :
           
