use std::mem::size_of;

pub trait Lengthy {
    fn len(&self) -> usize;
    fn set_len(&mut self, new_len: usize);
}

pub trait IntLog2 {
    fn log2_floor(self) -> Self;

    fn log2_ceil(self) -> Self;
}

impl IntLog2 for usize {
    fn log2_floor(self) -> usize {
        let mut x = self;
        for i in 0..size_of::<usize>() {
            x >>= 1;
            if x == 0 { return i; }
        }
        return size_of::<usize>() - 1;
    }

    fn log2_ceil(self) -> usize {
        let mut x = 1;
        for i in 0..size_of::<usize>() {
            if x >= self { return i; }
            x <<= 1;
        }
        return size_of::<usize>();
    }
}

#[cfg(test)]
mod tests {
    use super::IntLog2;

    use quickcheck::TestResult;

    quickcheck! {
        fn lg_floor(n: usize) -> TestResult {
            if n != 0 {
                TestResult::from_bool(n.log2_floor() == (n as f64).log2().floor() as usize)
            } else {
                TestResult::discard()
            }
        }
    }

    quickcheck! {
        fn lg_ceil(n: usize) -> TestResult {
            if n != 0 {
                TestResult::from_bool(n.log2_ceil() == (n as f64).log2().ceil() as usize)
            } else {
                TestResult::discard()
            }
        }
    }
}
