
/// Object references should implement this.
pub trait Reference: Copy {
    type Header: Header;

    /// Try to convert the reference to a raw pointer. Should produce `None` when self is immediate
    /// or null.
    fn ptr(self) -> Option<*const Self::Header>;

    /// Try to convert the reference to a mutable raw pointer. Should produce `None` when self is
    /// immediate or null.
    fn ptr_mut(self) -> Option<*mut Self::Header>;
}

/// Object headers should implement this.
pub trait Header {
    /// Is the object marked?
    fn is_marked(self) -> bool;

    /// Is the object a blob (consists of raw bytes instead of slots)?
    fn is_blob(self) -> bool;

    /// Get the raw tag of the object. Currently the tag is in the range [0, 16).
    fn tag(self) -> usize;

    /// Get the size of the object (in slots).
    fn len(self) -> usize;
}

/// Any data that should be allocated and managed by GC should implement this.
pub trait Object: Sized {
    /// The header representation.
    type Header: Header;

    /// The slot type that makes up the actual data.
    type Slot;

    /// Get the header of the object.
    fn header(&self) -> &Self::Header;

    /// Get a slice to the data of the object.
    fn data(&self) -> &[Self::Slot];
}
