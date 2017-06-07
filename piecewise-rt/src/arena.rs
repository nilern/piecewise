
/// A number such that SIZE = 1^SHIFT
pub const SHIFT: usize = 20;

/// An arena is SIZE bytes of memory
pub const SIZE: usize = 1 << SHIFT;

/// ptr & !MASK gives the address of the arena that ptr points into
pub const MASK: usize = SIZE - 1;

/// An arena holds CAPACITY allocatable blocks
pub const CAPACITY: usize = 252;
