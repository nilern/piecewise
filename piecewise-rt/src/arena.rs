use core::nonzero::NonZero;
use std::ptr;
use std::ptr::Unique;
use std::mem::{size_of, transmute};
#[cfg(unix)]
use nix::c_void;
#[cfg(unix)]
use nix::sys::mman::{mmap, munmap, PROT_READ, PROT_WRITE, MAP_ANON, MAP_PRIVATE};

use util::{Init, Uninitialized, CeilDiv};
use layout::{Arena, Block};
use descriptor::FreeRope;

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
        let m = n.ceil_div(Arena::CAPACITY);
        if self.allocated + m <= self.max_heap {
            unsafe {
                let ptr = (os_allocate(m) as usize
                           + Arena::SIZE
                           - Arena::CAPACITY*(size_of::<FreeRope>() + Block::SIZE))
                          as *mut Uninitialized<FreeRope>;
                self.allocated += m;
                let uptr = Unique::new(ptr);
                Some(FreeRope::init(uptr, NonZero::new(m * Arena::CAPACITY)))
            }
        } else {
            None
        }
    }

    // TODO: fn release(&mut self, ...)...
}

#[cfg(unix)]
unsafe fn os_allocate(n: usize) -> *mut () {
    let size = n*Arena::SIZE;
    let inflated_size = size + Arena::SIZE;

    let addr = mmap(ptr::null::<()>() as *mut c_void, inflated_size,
                    PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0)
                   .expect("out of memory");

    let offset = addr as usize & Arena::MASK;
    munmap(addr, Arena::SIZE - offset)
        .and_then(|()| if offset > 0 {
            munmap(addr.offset(inflated_size as isize).offset(-(offset as isize)), offset)
        } else {
            Ok(())
        })
        .expect("munmap() failed");

    addr.offset(Arena::SIZE as isize).offset(-(offset as isize)) as *mut ()
}

#[allow(dead_code)]
#[cfg(unix)]
unsafe fn os_free(ptr: *mut (), n: usize) {
    munmap(transmute(ptr), n).expect("munmap() failed");
}

#[cfg(test)]
mod tests {
    use super::{os_allocate, os_free};

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
