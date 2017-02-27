use std::convert::TryFrom;
use std::mem;
use std::mem::{size_of, transmute};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use gc;
use gc::Reference;

#[derive(Debug)]
pub struct TypeError(usize, usize);

#[derive(Debug)]
pub struct BoundsError(usize, usize);

// ------------------------------------------------------------------------------------------------

/// An object header.
#[derive(Debug, Clone, Copy)]
pub struct Header(usize);

impl Header {
    const MARK_MASK: usize = 0b0100;
    const BLOB_MASK: usize = 0b1000;
    const TAG_SHIFT: usize = 4;
    const TAG_MASK: usize = 0b1111;
    const ADMIN_MASK: usize = 0b1111;
    const LEN_SHIFT: usize = 8;
    const BLOB_SHIFT: usize = 3;
    const MARK_SHIFT: usize = 2;
    const FWD_SHIFT: usize = 1;

    pub fn new(marked: bool, blob: bool, tag: usize, len: usize) -> Header {
        Header(len << Header::LEN_SHIFT
               | tag << Header::TAG_SHIFT
               | (blob as usize) << Header::BLOB_SHIFT
               | (marked as usize) << Header::MARK_SHIFT
               | RawRef::HEADER_TAG)
    }
}

impl gc::Header for Header {
    fn is_marked(&self) -> bool { self.0 & Header::MARK_MASK != 0 }

    fn is_blob(&self) -> bool { self.0 & Header::BLOB_MASK != 0 }

    fn tag(&self) -> usize { self.0 >> Header::TAG_SHIFT & Header::TAG_MASK }

    fn len(&self) -> usize { self.0 >> Header::LEN_SHIFT }

    fn get_forward(&self) -> *mut Header {
        ((self.0 & !Header::ADMIN_MASK) >> Header::FWD_SHIFT) as *mut Header
    }

    fn set_mark(&mut self) { self.0 |= Header::MARK_MASK; }

    fn remove_mark(&mut self) { self.0 &= !Header::MARK_MASK; }

    fn set_forward_to(&mut self, dest: *mut Header) {
        self.0 = (dest as usize) << Header::FWD_SHIFT
               | 1 << Header::BLOB_SHIFT
               | 1 << Header::MARK_SHIFT
               | RawRef::HEADER_TAG;
    }
}

// ------------------------------------------------------------------------------------------------

/// A raw object reference, tagged pointer.
#[derive(Debug, Clone, Copy)]
pub struct RawRef(usize);

impl RawRef {
    const SHIFT: usize = 2;
    const MASK: usize = 0b11;
    const INT_TAG: usize = 0b00;
    const PTR_TAG: usize = 0b01;
    // const FLOAT_TAG: usize = 0b10;
    const HEADER_TAG: usize = 0b11;
}

impl From<isize> for RawRef {
    fn from(i: isize) -> RawRef {
        RawRef((i << RawRef::SHIFT) as usize)
    }
}

impl<H> From<*mut H> for RawRef where H: gc::Header {
    fn from(ptr: *mut H) -> RawRef {
        RawRef((ptr as usize) | RawRef::PTR_TAG)
    }
}

impl TryFrom<RawRef> for isize {
    type Err = TypeError;

    fn try_from(RawRef(i): RawRef) -> Result<isize, TypeError> {
        let tag = i & RawRef::MASK;
        if tag == RawRef::INT_TAG {
            Ok((i as isize) >> RawRef::SHIFT)
        } else {
            Err(TypeError(RawRef::INT_TAG, tag))
        }
    }
}

impl Default for RawRef {
    fn default() -> RawRef { RawRef(RawRef::PTR_TAG) }
}

impl Reference for RawRef {
    type Header = Header;

    fn from_mut_ptr(ptr: *mut Header) -> RawRef {
        From::from(ptr)
    }

    fn ptr(self) -> Option<*const Header> {
        <RawRef as gc::Reference>::ptr_mut(self).map(|ptr| ptr as *const Header)
    }

    fn ptr_mut(self) -> Option<*mut Header> {
        if self.0 & RawRef::MASK == RawRef::PTR_TAG {
            let ptr: *mut Header = unsafe { mem::transmute(self.0) };
            if !ptr.is_null() {
                return Some(ptr);
            }
        }
        None
    }
}

// ------------------------------------------------------------------------------------------------

/// Statically typed version of RawRef
#[derive(Debug, Copy)]
pub struct TypedRef<T>(RawRef, PhantomData<T>) where T: Sized;

impl<T> Clone for TypedRef<T> where T: Sized {
    fn clone(&self) -> TypedRef<T> {
        TypedRef(self.0, Default::default())
    }
}

impl<T> From<RawRef> for TypedRef<T> {
    fn from(rref: RawRef) -> TypedRef<T> { TypedRef(rref, Default::default()) }
}

impl<T> Deref for TypedRef<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { transmute(self.0.ptr().unwrap().offset(1)) }
    }
}

impl<T> DerefMut for TypedRef<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { transmute(self.0.ptr_mut().unwrap().offset(1)) }
    }
}

impl<T> From<TypedRef<T>> for RawRef {
    fn from(tref: TypedRef<T>) -> RawRef {
        tref.0
    }
}

impl From<TypedRef<isize>> for isize {
    fn from(tref: TypedRef<isize>) -> isize {
        TryFrom::try_from(tref.0).unwrap()
    }
}

// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct ByteArray;

impl ByteArray {
    const TAG: usize = 0;

    pub unsafe fn len(&self) -> usize {
        let header = transmute::<&ByteArray, *const Header>(self).offset(-1);
        gc::Header::len(&*header)
    }

    pub unsafe fn get<T>(&self, index: usize) -> Result<T, BoundsError> where T: Copy {
        // FIXME: check that the entire value can be retrieved
        if index < self.len() {
            let ptr: *const T = transmute(self);
            Ok(*ptr.offset(index as isize))
        } else {
            Err(BoundsError(self.len(), index))
        }
    }
}

impl gc::Object for ByteArray {
    type Header = Header;
}

impl gc::UnSizedFlatObject for ByteArray {
    fn header(len: usize) -> Header {
        Header::new(false, true, ByteArray::TAG, len)
    }
}

// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Tuple;

impl Tuple {
    const TAG: usize = 1;

    pub unsafe fn len(&self) -> usize {
        let header = transmute::<&Tuple, *const Header>(self).offset(-1);
        gc::Header::len(&*header)
    }

    pub unsafe fn get(&self, index: usize) -> Result<RawRef, BoundsError> {
        if index < self.len() {
            let ptr: *const RawRef = transmute(self);
            Ok(*ptr.offset(index as isize))
        } else {
            Err(BoundsError(self.len(), index))
        }
    }
}

impl gc::Object for Tuple {
    type Header = Header;
}

impl gc::UnSizedPointyObject for Tuple {
    fn header(len: usize) -> Header {
        Header::new(false, true, Tuple::TAG, len)
    }
}

// ------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct CodeObject {
    pub code: TypedRef<ByteArray>,
    pub consts: TypedRef<Tuple>,
    pub reg_req: TypedRef<isize>,
    pub cobs: TypedRef<Tuple>
}

impl CodeObject {
    const TAG: usize = 2;
}

impl gc::Object for CodeObject {
    type Header = Header;
}

impl gc::SizedPointyObject for CodeObject {
    fn header() -> Header {
        Header::new(false, false, CodeObject::TAG, size_of::<CodeObject>() / size_of::<RawRef>())
    }
}

// ------------------------------------------------------------------------------------------------


#[derive(Debug)]
pub struct Closure {
    pub cob: TypedRef<CodeObject>
}

impl Closure {
    const TAG: usize = 3;
}

impl gc::Object for Closure {
    type Header = Header;
}

impl gc::SizedPointyObject for Closure {
    fn header() -> Header {
        Header::new(false, false, Closure::TAG, size_of::<Closure>() / size_of::<RawRef>())
    }
}

// ------------------------------------------------------------------------------------------------

// /// The tag pair implementation.
// #[repr(C)]
// pub struct TagPair {
//     header: Header,
//     left: RawRef,
//     right: RawRef
// }
//
// impl gc::Object for TagPair {
//     type Header = Header;
//     type Slot = RawRef;
//
//     fn header(&self) -> &Header { &self.header }
//
//     fn header_mut(&mut self) -> &mut Self::Header { &mut self.header }
//
//     fn set_header(&mut self, header: Header) { self.header = header; }
//
//     fn data(&self) -> &[RawRef] {
//         unsafe {
//             slice::from_raw_parts(mem::transmute::<&RawRef, *const RawRef>(&self.left),
//                                   gc::Header::len(&self.header))
//         }
//     }
// }

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
