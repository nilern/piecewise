use std::mem::transmute;
use std::ptr;
use nix::c_void;
#[cfg(unix)]
use nix::sys::mman::{mmap, munmap, PROT_READ, PROT_WRITE, MAP_ANON, MAP_PRIVATE};
use intrusive_collections::{UnsafeRef, LinkedListLink, LinkedList};

use block;
use arena;
use descriptor::Descriptor;

pub struct ArenaArr {
    link: LinkedListLink,
    len: usize
}

intrusive_adapter!(pub ArrAdapter = UnsafeRef<ArenaArr>: ArenaArr { link: LinkedListLink });

impl ArenaArr {
    fn new(len: usize) -> *mut ArenaArr {
        unsafe { ArenaArr::init(os_allocate(len), len) }
    }

    fn len(&self) -> usize { self.len }

    fn split_off(&mut self, n: usize) -> *mut ArenaArr {
        let ptr = self.offset(self.len() - n);
        unsafe { ArenaArr::init_descr(ptr, n) }
    }

    fn offset(&self, n: usize) -> *mut () {
        (unsafe { transmute::<_, usize>(self) } + n*arena::SIZE) as _
    }

    unsafe fn init(start: *mut (), len: usize) -> *mut ArenaArr {
        let descr = (start as usize + arena::SIZE
                     - arena::CAPACITY*(block::SIZE + Descriptor::SIZE)) as _;
        ArenaArr::init_descr(descr, len)
    }

    unsafe fn init_descr(descr: *mut (), len: usize) -> *mut ArenaArr {
        let descr = descr as _;
        ptr::write(descr, Descriptor::ArenaArr(ArenaArr {
            link: LinkedListLink::default(),
            len: len
        }));

        if let Descriptor::ArenaArr(ref arr) = *descr {
            transmute(arr)
        } else {
            unreachable!()
        }
    }
}

/// Arena allocator (allocates ArenaArr:s)
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

    /// Allocate an ArenaArr of length `n`.
    pub fn allocate(&mut self, n: usize) -> *mut ArenaArr {
        let mut best: Option<&ArenaArr> = None;
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
            unsafe { (*transmute::<_, *mut ArenaArr>(descr)).split_off(n) }
        } else {
            ArenaArr::new(n)
        }
    }
}

#[cfg(unix)]
unsafe fn os_allocate(n: usize) -> *mut () {
    // basically just wraps `mmap()`

    let size = n*arena::SIZE;
    let inflated_size = size + arena::SIZE;

    let addr = mmap(ptr::null::<()>() as *mut c_void, inflated_size,
                    PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0)
                   .expect("out of memory");

    let offset = addr as usize & arena::MASK;
    munmap(addr, arena::SIZE - offset)
        .and_then(|_| {
            munmap(addr.offset(inflated_size as isize).offset(-(offset as isize)), offset)
        })
        .expect("munmap() failed");

    addr.offset(arena::SIZE as isize).offset(-(offset as isize)) as *mut ()
}
