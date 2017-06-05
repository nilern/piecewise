use arena;
use util::IntLog2;

const SHIFT: usize = 12;
pub const SIZE: usize = 1 << SHIFT;

enum Descriptor {
    Links(Links)
}

impl Descriptor {
    #[cfg(target_pointer_width = "64")]
    const SHIFT: usize = 6;

    const SIZE: usize = 1 << Descriptor::SHIFT;

    fn from_ptr(ptr: *const ()) -> *mut Descriptor {
        let index = (ptr as usize & arena::MASK) >> SHIFT;
        let byte_index = index >> SHIFT << Descriptor::SHIFT;
        (byte_index as usize & !arena::MASK | byte_index) as _
    }

    fn len(&self) -> usize {
        match self {
            &Descriptor::Links(Links {len, ..}) => len
        }
    }

    fn push_front(&mut self, other: &mut Descriptor) {
        unimplemented!()
    }

    fn pop_front(&mut self) -> Option<*mut Descriptor> {
        unimplemented!()
    }
}

struct Links {
    len: usize,
    next: Option<*mut Descriptor>,
    prev: Option<*mut Descriptor>
}

impl Links {
    fn push_front(&mut self, other: &mut Descriptor) {
        unimplemented!()
    }

    fn pop_front(&mut self) -> Option<*mut Descriptor> {
        unimplemented!()
    }
}

struct Allocator {
    arena_allocator: arena::Allocator,
    arena_list: Option<*mut arena::Descriptor>,
    grouplists: [Option<*mut Descriptor>; Allocator::NLISTS]
}

impl Allocator {
    const NLISTS: usize = arena::SHIFT - SHIFT;

    fn new() -> Allocator {
        Allocator {
            arena_allocator: arena::Allocator::new(),
            arena_list: None,
            grouplists: [None; Allocator::NLISTS]
        }
    }

    fn allocate(&mut self, n: usize) -> *mut Descriptor {
        if n <= arena::CAPACITY {
            unimplemented!()
        } else {
            let start = n.log2_ceil();
            if let Some(i) = self.grouplists[start..].iter().position(Option::is_some) {
                let descr = unsafe { &mut *self.grouplists[i].unwrap() };
                match descr.len() {
                    len if len == n => {
                        descr.pop_front().unwrap()
                    },
                    len if len > n => {
                        unimplemented!()
                    },
                    _ => panic!("grouplists corrupted")
                }
            } else {
                unimplemented!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Descriptor;
    use std::mem;

    #[test]
    fn descriptor_size() {
        assert_eq!(mem::size_of::<Descriptor>(), Descriptor::SIZE);
    }
}
