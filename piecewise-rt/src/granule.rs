use std::mem::size_of;

use util::CeilDiv;

pub struct Granule(usize);

#[derive(Debug, Clone, Copy)]
pub struct GSize(usize);

impl GSize {
    pub fn of<T>() -> Self {
        GSize(size_of::<T>().ceil_div(size_of::<Granule>()))
    }

    pub fn next_aligned(align: GSize, ptr: *const Granule) -> *const Granule {
        let stride = align.0 * size_of::<Granule>();
        let rem = ptr as usize % stride;
        let inv_rem = stride - rem;
        unsafe { ptr.offset(inv_rem as isize) }
    }
}
