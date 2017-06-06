use std::ptr;
use nix::c_void;
#[cfg(unix)]
use nix::sys::mman::{mmap, munmap, PROT_READ, PROT_WRITE, MAP_ANON, MAP_PRIVATE};

pub const SHIFT: usize = 20;
pub const SIZE: usize = 1 << SHIFT;
pub const MASK: usize = SIZE - 1;
pub const CAPACITY: usize = 252;

/// Arena allocator (allocates ArenaArr:s)
pub struct Allocator {
    //arena_arr_list: FreeList<...>
}

impl Allocator {
    /// Create a new arena allocator.
    pub fn new() -> Allocator {
        Allocator {
            //arena_arr_list: FreeList::new()
        }
    }

    /// Allocate an ArenaArr of length `n`.
    pub fn allocate(&mut self, n: usize) -> *mut () {
        os_allocate(n)
    }
}

#[cfg(unix)]
fn os_allocate(n: usize) -> *mut () {
    // basically just wraps `mmap()`

    let size = n*SIZE;
    let inflated_size = size + SIZE;

    unsafe {
        let addr = mmap(ptr::null::<()>() as *mut c_void, inflated_size,
                        PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0)
                       .expect("out of memory");

        let offset = addr as usize & MASK;
        munmap(addr, SIZE - offset)
            .and_then(|_| {
                munmap(addr.offset(inflated_size as isize).offset(-(offset as isize)), offset)
            })
            .expect("munmap() failed");

        addr.offset(SIZE as isize).offset(-(offset as isize)) as *mut ()
    }
}

#[cfg(test)]
mod tests {
    // use std::mem;
    //
    // use super::{Descriptor, SIZE, CAPACITY};
    // use block;
    //
    // #[test]
    // fn descriptor_size() {
    //     assert!(mem::size_of::<Descriptor>() <= mem::size_of::<block::Descriptor>());
    // }
    //
    // #[test]
    // fn arena_capacity() {
    //     assert!(CAPACITY*(block::SIZE + mem::size_of::<block::Descriptor>()) <= SIZE);
    // }
}
