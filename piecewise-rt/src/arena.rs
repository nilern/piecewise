use core::nonzero::NonZero;
use std::ptr;
use std::ptr::Unique;
use std::mem::{size_of, transmute};
#[cfg(unix)]
use nix::c_void;
#[cfg(unix)]
use nix::sys::mman::{mmap, munmap, PROT_READ, PROT_WRITE, MAP_ANON, MAP_PRIVATE};

use util::{Init, Uninitialized, CeilDiv};
use block::{self, Block, Descriptor};
use freerope::FreeRope;

/// A number such that SIZE = 1^SHIFT
pub const SHIFT: usize = 20;

/// An arena is SIZE bytes of memory
pub const SIZE: usize = 1 << SHIFT;

/// ptr & !MASK gives the address of the arena that ptr points into
pub const MASK: usize = SIZE - 1;

/// An arena holds CAPACITY allocatable blocks
pub const CAPACITY: usize = 252;

/// At the start of an arena there are SLACK uninitialized bytes. The first block descriptor is at
/// address `arena_start + SLACK`.
pub const SLACK: usize = SIZE - CAPACITY*(Descriptor::SIZE + block::SIZE);

#[repr(C)]
pub struct Arena {
    _slack: [u8; SLACK],
    pub descriptors: [Descriptor; CAPACITY],
    pub blocks: [Block; CAPACITY]
}

impl Arena {
    pub fn containing<T>(ptr: *const T) -> *const Arena {
        (ptr as usize & !self::MASK) as _
    }

    pub fn descriptor_index<T>(&self, descr: &T) -> usize { // TODO: ...<T: Descriptor>(...
        unsafe {
            let daddr: usize = transmute(descr);
            let ds_addr: usize = transmute(&self.descriptors);
            (daddr - ds_addr) / Descriptor::SIZE
        }
    }
}

pub struct ArenaAllocator {
    max_heap: usize,
    allocated: usize
}

impl ArenaAllocator {
    pub fn new(max_heap: usize) -> Self {
        ArenaAllocator {
            max_heap: max_heap,
            allocated: 0
        }
    }

    pub fn allocate_at_least(&mut self, n: NonZero<usize>) -> Option<Unique<FreeRope>> {
        let m = n.ceil_div(CAPACITY);
        if self.allocated + m <= self.max_heap {
            unsafe {
                let ptr = (os_allocate(m) as usize
                           + SIZE
                           - CAPACITY*(size_of::<FreeRope>() + block::SIZE))
                          as *mut Uninitialized<FreeRope>;
                self.allocated += m;
                let uptr = Unique::new(ptr);
                Some(FreeRope::init(uptr, NonZero::new(m * CAPACITY)))
            }
        } else {
            None
        }
    }

    // TODO: fn release(&mut self, ...)...
}

#[cfg(unix)]
unsafe fn os_allocate(n: usize) -> *mut () {
    let size = n*SIZE;
    let inflated_size = size + SIZE;

    let addr = mmap(ptr::null::<()>() as *mut c_void, inflated_size,
                    PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0)
                   .expect("out of memory");

    let offset = addr as usize & MASK;
    munmap(addr, SIZE - offset)
        .and_then(|()| if offset > 0 {
            munmap(addr.offset(inflated_size as isize).offset(-(offset as isize)), offset)
        } else {
            Ok(())
        })
        .expect("munmap() failed");

    addr.offset(SIZE as isize).offset(-(offset as isize)) as *mut ()
}

#[allow(dead_code)]
#[cfg(unix)]
unsafe fn os_free(ptr: *mut (), n: usize) {
    munmap(transmute(ptr), n).expect("munmap() failed");
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::{Arena, SIZE, os_allocate, os_free};

    #[test]
    fn arena_size() {
        assert_eq!(size_of::<Arena>(), SIZE);
    }

    #[test]
    fn os_layer() {
        unsafe {
            let blobs: Vec<(usize, *mut ())> = (1..100).map(|n| (n, os_allocate(n))).collect();
            for (n, ptr) in blobs {
                os_free(ptr, n);
            }
        }
    }
}
