use core::nonzero::NonZero;
use std::fmt::Debug;
use std::mem::transmute;
use std::ptr::Unique;
use std::cmp::Ordering;
use std::ops::Range;
use intrusive_collections::{Adapter, LinkedList, LinkedListLink, IntrusivePointer};

use util::{Init, Lengthy, SplitOff, Uninitialized};

// FIXME: what if the remainder cannot hold a N::Value?

pub struct FirstFitHead<N> where N: Adapter<Link = LinkedListLink> {
    list: LinkedList<N>,
    bounds: Range<usize>
}

impl<N> FirstFitHead<N> where N: Adapter<Link = LinkedListLink> + Default,
                              N::Value: Sized + Init + Lengthy,
                              N::Pointer: Debug
{
    pub fn new(bounds: Range<usize>) -> Self {
        FirstFitHead {
            list: LinkedList::new(N::default()),
            bounds: bounds
        }
    }

    pub fn allocate_at_least<R>(&mut self, n: NonZero<usize>) -> Option<R>
        where N::Value: SplitOff<R>
    {
        let mut cursor = self.list.cursor_mut();
        while let Some(node) = cursor.get() {
            match node.len().cmp(&*n) {
                Ordering::Equal =>
                    return cursor.remove()
                                 .map(|ptr| unsafe { Unique::new(ptr.into_raw() as _) }),
                Ordering::Greater => unsafe {
                    let new_node = N::Pointer::from_raw(*node.split_off(*n) as _);
                    let ptr = cursor.replace_with(new_node).unwrap().into_raw() as _;
                    return Some(Unique::new(ptr));
                },
                Ordering::Less => cursor.move_next()
            }
        }
        None
    }

    pub fn release(&mut self, uptr: Unique<Uninitialized<N::Value>>, n: NonZero<usize>) {
        self.list.push_front(unsafe { N::Pointer::from_raw(*N::Value::init(uptr, n)) })
    }
}

// ================================================================================================

pub struct FirstFitTail<N> where N: Adapter<Link = LinkedListLink> {
    list: LinkedList<N>,
    bounds: Range<usize>
}

impl<N> FirstFitTail<N> where N: Adapter<Link = LinkedListLink> + Default,
                              N::Value: Sized + Init + Lengthy,
                              N::Pointer: Debug
{
    pub fn new(bounds: Range<usize>) -> Self {
        FirstFitTail {
            list: LinkedList::new(N::default()),
            bounds: bounds
        }
    }

    pub fn allocate_at_least<R>(&mut self, n: NonZero<usize>) -> Option<R>
        where N::Value: SplitOff<R>
    {
        let mut cursor = self.list.cursor_mut();
        while let Some(node) = cursor.get() {
            match node.len().cmp(&*n) {
                Ordering::Equal =>
                    return cursor.remove()
                                 .map(|ptr| unsafe { Unique::new(ptr.into_raw() as _) }),
                Ordering::Greater => return Some(unsafe { transmute(node.split_off(*n)) }),
                Ordering::Less => cursor.move_next()
            }
        }
        None
    }

    pub fn release(&mut self, uptr: Unique<Uninitialized<N::Value>>, n: NonZero<usize>) {
        self.list.push_front(unsafe { N::Pointer::from_raw(*N::Value::init(uptr, n)) })
    }
}

// ================================================================================================

pub struct BestFitHead<N> where N: Adapter<Link = LinkedListLink> {
    list: LinkedList<N>,
    bounds: Range<usize>
}

impl<N> BestFitHead<N> where N: Adapter<Link = LinkedListLink> + Default,
                             N::Value: Sized + Init + Lengthy,
                             N::Pointer: Debug
{
    pub fn new(bounds: Range<usize>) -> Self {
        BestFitHead {
            list: LinkedList::new(N::default()),
            bounds: bounds
        }
    }

    pub fn allocate_at_least<R>(&mut self, n: NonZero<usize>) -> Option<R>
        where N::Value: SplitOff<R>
    {
        let mut best: Option<*const N::Value> = None;
        {
            let mut cursor = self.list.cursor_mut();
            while let Some(node) = cursor.get() {
                if best.is_none() {
                    best = Some(unsafe { transmute(node) });
                } else {
                    match node.len().cmp(&*n) {
                        Ordering::Equal =>
                            return cursor.remove()
                                         .map(|ptr| unsafe { Unique::new(ptr.into_raw() as _) }),
                        Ordering::Greater if node.len() < unsafe { (*best.unwrap()).len() } => {
                            best = Some(unsafe { transmute(node) });
                        },
                        _ => {}
                    }
                }
                cursor.move_next()
            }
        }
        best.map(|node| unsafe {
            let mut cursor = self.list.cursor_mut_from_ptr(node);
            let new_node = N::Pointer::from_raw(*(*node).split_off(*n) as _);
            let ptr = cursor.replace_with(new_node).unwrap().into_raw() as _;
            Unique::new(ptr)
        })
    }

    pub fn release(&mut self, uptr: Unique<Uninitialized<N::Value>>, n: NonZero<usize>) {
        self.list.push_front(unsafe { N::Pointer::from_raw(*N::Value::init(uptr, n)) })
    }
}
