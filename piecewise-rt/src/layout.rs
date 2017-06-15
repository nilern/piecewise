use std::mem::{size_of, transmute};

use util::CeilDiv;

// ================================================================================================

#[repr(C)]
pub struct Arena {
    _slack: [u8; Arena::SLACK],
    pub descriptors: [[u8; DESCR_SIZE]; Arena::CAPACITY],
    pub blocks: [Block; Arena::CAPACITY]
}

impl Arena {
    /// A number such that SIZE = 1^SHIFT
    pub const SHIFT: usize = 20;

    /// An arena is SIZE bytes of memory
    pub const SIZE: usize = 1 << Self::SHIFT;

    /// ptr & !MASK gives the address of the arena that ptr points into
    pub const MASK: usize = Self::SIZE - 1;

    /// An arena holds CAPACITY allocatable blocks
    pub const CAPACITY: usize = 252;

    /// At the start of an arena there are SLACK uninitialized bytes. The first block descriptor is
    /// at address `arena_start + SLACK`.
    pub const SLACK: usize = Self::SIZE - Self::CAPACITY*(DESCR_SIZE + Block::SIZE);

    pub fn containing<T>(ptr: *const T) -> *const Arena {
        (ptr as usize & !Self::MASK) as _
    }

    pub fn descriptor_index<T>(&self, descr: &T) -> usize { // TODO: ...<T: Descriptor>(...
        unsafe {
            let daddr: usize = transmute(descr);
            let ds_addr: usize = transmute(&self.descriptors);
            (daddr - ds_addr) / DESCR_SIZE
        }
    }
}

// ================================================================================================

pub struct Block([usize; Block::WSIZE]);

impl Block {
    /// A number such that SIZE = 1^SHIFT
    pub const SHIFT: usize = 12;

    /// A block is SIZE bytes of memory
    pub const SIZE: usize = 1 << Self::SHIFT;

    pub const WSIZE: usize = Self::SIZE / Granule::SIZE;
}

// ================================================================================================

pub struct Granule(usize);

impl Granule {
    #[cfg(target_pointer_width = "64")]
    const SIZE: usize = 8;
}

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

// ================================================================================================

#[cfg(target_pointer_width = "64")]
pub const DESCR_SHIFT: usize = 6;

pub const DESCR_SIZE: usize = 1 << DESCR_SHIFT;

pub const DESCR_MASK: usize = DESCR_SIZE - 1;

// ================================================================================================

pub type Markmap = [u8; Block::WSIZE];

// ================================================================================================

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::{Arena, Block, Granule, DESCR_SIZE, Markmap};

    #[test]
    fn arena_size() {
        assert_eq!(size_of::<Arena>(), Arena::SIZE);
    }

    #[test]
    fn block_size() {
        assert_eq!(size_of::<Block>(), Block::SIZE);
    }

    #[test]
    fn granule_size() {
        assert_eq!(size_of::<Granule>(), size_of::<usize>());
    }

    #[test]
    fn descriptor_size() {
        assert_eq!(DESCR_SIZE, 8*size_of::<usize>());
    }

    #[test]
    fn markmap_size() {
        assert_eq!(size_of::<Markmap>(), Block::WSIZE);
    }
}
