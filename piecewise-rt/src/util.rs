
pub trait IntLog2 {
    fn log2_floor(self) -> Self;

    fn log2_ceil(self) -> Self;
}

impl IntLog2 for usize {
    fn log2_floor(self) -> usize {
        unimplemented!()
    }

    fn log2_ceil(self) -> usize {
        unimplemented!()
    }
}
