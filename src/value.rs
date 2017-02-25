use gc;

use std::slice;
use std::convert::TryFrom;
use std::mem;

const MARK_MASK: usize = 0b0100;
const BLOB_MASK: usize = 0b1000;
const TAG_SHIFT: usize = 4;
const TAG_MASK: usize = 0b1111;
const LEN_SHIFT: usize = 8;

#[derive(Debug)]
pub struct TypeError(usize, usize);

/// An object header.
#[derive(Clone, Copy)]
pub struct Header(usize);

impl gc::Header for Header {
    fn is_marked(self) -> bool { self.0 & MARK_MASK != 0 }

    fn is_blob(self) -> bool { self.0 & BLOB_MASK != 0 }

    fn tag(self) -> usize { self.0 >> TAG_SHIFT & TAG_MASK }

    fn len(self) -> usize { self.0 >> LEN_SHIFT }
}

/// A raw object reference, tagged pointer.
#[derive(Debug, Clone, Copy)]
pub struct RawRef(usize);

impl From<isize> for RawRef {
    fn from(i: isize) -> RawRef {
        RawRef((i as usize) << 2)
    }
}

impl TryFrom<RawRef> for isize {
    type Err = TypeError;

    fn try_from(RawRef(i): RawRef) -> Result<isize, TypeError> {
        if i & 0b11 == 0b00 {
            Ok(i as isize >> 2)
        } else {
            Err(TypeError(0b00, i & 0b11))
        }
    }
}

impl gc::Reference for RawRef {
    type Header = Header;

    fn ptr(self) -> Option<*const Header> {
        self.ptr_mut().map(|ptr| ptr as *const Header)
    }

    fn ptr_mut(self) -> Option<*mut Header> {
        if self.0 & 0b11 == 0b01 {
            let ptr: *mut Header = unsafe { mem::transmute(self.0 - 1) };
            if !ptr.is_null() {
                return Some(ptr);
            }
        }
        None
    }
}

/// The tag pair implementation.
#[repr(C)]
pub struct TagPair {
    header: Header,
    left: RawRef,
    right: RawRef
}

impl gc::Object for TagPair {
    type Header = Header;
    type Slot = RawRef;

    fn header(&self) -> &Header { &self.header }

    fn data(&self) -> &[RawRef] {
        unsafe {
            slice::from_raw_parts(mem::transmute::<&RawRef, *const RawRef>(&self.left),
                                  gc::Header::len(self.header))
        }
    }
}

// enum BaseTag {
//     Int = 0b00,
//     Allocated = 0b01,
//     Float = 0b10,
//     Header = 0b11
// }
//
// enum Tag {
//     Int = 0b00,
//     Float = 0b10,
//     Header = 0b11,
//     TagPair = 0b0111,
//     Tuple = 0b1011,
//     Blob = 0b1111
// }
//
// #[derive(Clone, Copy)]
// struct ValueRef(usize);
//
// impl ValueRef {
//     fn unpack(self) -> ValueProj {
//         match self.tag() {
//             Tag::Int => ValueProj::Int(self.0),
//             Tag::Float => ValueProj::Float(mem::transmute(self.0)),
//             Tag::Header => ValueProj::Header { .. },
//             Tag::TagPair => ValueProj::TagPair { .. }
//             Tag::Tuple => ValueProj::TagPair { .. }
//             Tag::Blob => ValueProj::TagPair { .. }
//         }
//     }
//
//     fn base_tag(self) -> BaseTag {
//         match self.0 & 0b11 {
//             0b00 => BaseTag::Int,
//             0b01 => BaseTag::Allocated,
//             0b10 => BaseTag::Float,
//             0b11 => BaseTag::Header,
//             _ => unreachable!()
//         }
//     }
//
//     fn tag(self) -> Tag {
//         match self.base_tag() {
//             BaseTag::Int => Tag::Int,
//             BaseTag::Float => Tag::Float,
//             BaseTag::Header => Tag::Header,
//             BaseTag::Allocated =>
//                 match unsafe { *(self.0 as *const usize) } >> 4 & 0b1111 {
//                     0b01 => Tag::TagPair,
//                     0b10 => Tag::Tuple,
//                     0b11 => Tag::Blob,
//                     _ => unimplemented!()
//                 }
//         }
//     }
// }
//
// enum ValueProj {
//     Int(isize),
//     Float(f64),
//     Header(Header),
//     Tagpair(ValueRef, ValueRef)
// }
