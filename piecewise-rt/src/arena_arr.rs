use std::mem::transmute;
use std::ptr;
use nix::c_void;
#[cfg(unix)]
use nix::sys::mman::{mmap, munmap, PROT_READ, PROT_WRITE, MAP_ANON, MAP_PRIVATE};
use intrusive_collections::{UnsafeRef, LinkedListLink, LinkedList};

use block;
use arena;

pub struct Descriptor {
    link: LinkedListLink,
    len: usize
}

intrusive_adapter!(pub ArrAdapter = UnsafeRef<Descriptor>: Descriptor { link: LinkedListLink });

impl Descriptor {
    #[cfg(target_pointer_width = "64")]
    const SHIFT: usize = 6;

    pub const SIZE: usize = 1 << Descriptor::SHIFT;

    pub const MASK: usize = Descriptor::SIZE - 1;

    fn new(len: usize) -> *mut Descriptor {
        let start = unsafe { os_allocate(len) };
        let descr = (start as usize + arena::SIZE
                     - arena::CAPACITY*(block::SIZE + Descriptor::SIZE)) as _;
        unsafe { Descriptor::init(descr, len) }
    }

    fn len(&self) -> usize { self.len }

    fn split_off(&mut self, n: usize) -> *mut Descriptor {
        let ptr = self.offset(self.len() - n);
        unsafe { Descriptor::init(ptr, n) }
    }

    fn offset(&self, n: usize) -> *mut () {
        (unsafe { transmute::<_, usize>(self) } + n*arena::SIZE) as _
    }

    unsafe fn init(descr: *mut (), len: usize) -> *mut Descriptor {
        let descr = descr as _;
        ptr::write(descr, Descriptor {
            link: LinkedListLink::default(),
            len: len
        });
        descr
    }
}

/// Arena allocator (allocates Descriptor:s)
pub struct Allocator {
    arena_arr_list: LinkedList<ArrAdapter>
}

impl Allocator {
    /// Create a new arena allocator.
    pub fn new() -> Allocator {
        Allocator {
            arena_arr_list: LinkedList::new(ArrAdapter::new())
        }
    }

    /// Allocate an Descriptor of length `n`.
    pub fn allocate(&mut self, n: usize) -> *mut Descriptor {
        let mut best: Option<&Descriptor> = None;
        let mut cursor = self.arena_arr_list.cursor_mut();

        loop {
            match (best, cursor.get()) {
                (_, None) => break,
                (_, Some(d)) if d.len() == n => {
                    cursor.remove();
                    return unsafe { transmute(d) };
                },
                (Some(b), Some(d)) if d.len() > n && d.len() < b.len() =>
                    best = cursor.get(),
                (Some(_), Some(_)) => {},
                (None, descr) =>
                    best = descr
            }
            cursor.move_next();
        }

        if let Some(descr) = best {
            unsafe { (*transmute::<_, *mut Descriptor>(descr)).split_off(n) }
        } else {
            Descriptor::new(n)
        }
    }
}

#[cfg(unix)]
unsafe fn os_allocate(n: usize) -> *mut () {
    let size = n*arena::SIZE;
    let inflated_size = size + arena::SIZE;

    let addr = mmap(ptr::null::<()>() as *mut c_void, inflated_size,
                    PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0)
                   .expect("out of memory");

    let offset = addr as usize & arena::MASK;
    munmap(addr, arena::SIZE - offset)
        .and_then(|()| if offset > 0 {
            munmap(addr.offset(inflated_size as isize).offset(-(offset as isize)), offset)
        } else {
            Ok(())
        })
        .expect("munmap() failed");

    addr.offset(arena::SIZE as isize).offset(-(offset as isize)) as *mut ()
}

#[cfg(unix)]
unsafe fn os_free(ptr: *mut (), n: usize) {
    munmap(transmute(ptr), n).expect("munmap() failed");
}

#[cfg(test)]
mod tests {
    use super::{os_allocate, os_free, Descriptor};
    use std::mem;

    #[test]
    fn os_layer() {
        unsafe {
            let blobs: Vec<(usize, *mut ())> = (1..100).map(|n| (n, os_allocate(n))).collect();
            for (n, ptr) in blobs {
                os_free(ptr, n);
            }
        }
    }

    #[test]
    fn descriptor_size() {
        assert!(mem::size_of::<Descriptor>() <= Descriptor::SIZE);
    }
}
