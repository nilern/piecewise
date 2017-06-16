#![feature(nonzero, unique, shared, associated_consts)]

extern crate core;
extern crate nix;
#[macro_use]
extern crate intrusive_collections;

mod util;
mod layout;
mod arena;
mod block;
mod descriptor;
mod object_model;
mod mark_n_sweep;

use core::nonzero::NonZero;
use std::ptr::Unique;
use std::sync::Mutex;

use util::Uninitialized;
use object_model::ValueRef;
use mark_n_sweep::Generation;

pub type Heap = Generation;

/// Create a new heap with a maximum size of `max_heap` MiB.
#[no_mangle]
pub extern "C" fn pcws_new_heap(max_heap: usize) -> *mut Mutex<Heap> {
    Box::into_raw(Box::new(Mutex::new(Generation::new(max_heap))))
}

/// Heap destructor.
/// # Safety
/// This must be called exactly once per heap.
#[no_mangle]
pub unsafe extern "C" fn pcws_destroy_heap(heap: *mut Mutex<Heap>) {
    Box::from_raw(heap);
}

/// Allocate `wsize` words of memory, aligned to `walign` words, from `heap`. Returns `None` if
/// heap is so full it should be collected.
/// # Safety
/// `walign` and `wsize` must be nonzero.
#[no_mangle]
pub unsafe extern "C" fn pcws_allocate(heap: *mut Mutex<Heap>, walign: usize, wsize: usize)
    -> Option<Unique<Uninitialized<usize>>>
{
    (*heap).lock().expect("poisoned heap lock")
           .allocate(NonZero::new(walign), NonZero::new(wsize))
}

/// Mark a root and return the updated root reference
/// # Safety
/// You must replace the old root with the return value (to enable moving GC).
#[no_mangle]
pub unsafe extern "C" fn pcws_mark_root(heap: *mut Mutex<Heap>, root: ValueRef) -> ValueRef {
    (*heap).lock().expect("poisoned heap lock")
           .mark_ref(root);
    root
}

/// Collect garbage from `heap`.
/// # Safety
/// All roots must be marked (with `pcws_mark_root`) before attempting this (in order to preserve
/// the GC invariant that only garbage gets collected).
#[no_mangle]
pub unsafe extern "C" fn pcws_collect(heap: *mut Mutex<Heap>) {
    (*heap).lock().expect("poisoned heap lock").collect()
}
