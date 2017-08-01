#![feature(shared, associated_consts, test)]

extern crate core;
extern crate piecewise_rt;
extern crate test;

use std::mem::{size_of, transmute};
use std::ptr::{self, Shared};
use std::sync::Mutex;
use test::Bencher;

use piecewise_rt::{Heap, ValueRef, PointyObject,
                   pcws_new_heap, pcws_destroy_heap, pcws_allocate, pcws_mark_root, pcws_collect};

#[repr(C)]
struct Node {
    header: usize,
    left: ValueRef,
    right: ValueRef,
    i: ValueRef,
    j: ValueRef
}

struct GCBench {
    heap: *mut Mutex<Heap>,
    stack: Vec<ValueRef>
}

impl GCBench {
    const MIN_DEPTH: usize = 4;
    const MAX_DEPTH: usize = 16;

    fn new() -> Self {
        GCBench {
            heap: pcws_new_heap(32),
            stack: Vec::new()
        }
    }

    fn new_node(&mut self, mut l: ValueRef, mut r: ValueRef) -> ValueRef {
        unsafe {
            let ptr = *pcws_allocate(self.heap, 1, size_of::<Node>() / size_of::<ValueRef>())
                           .or_else(|| {
                               l = pcws_mark_root(self.heap, l);
                               r = pcws_mark_root(self.heap, r);
                               for root in self.stack.iter_mut() {
                                   *root = pcws_mark_root(self.heap, *root);
                               }
                               pcws_collect(self.heap);
                               pcws_allocate(self.heap, 1,
                                             size_of::<Node>() / size_of::<ValueRef>())
                           })
                           .expect("Heap exhausted!") as *mut Node;
            ptr::write(ptr, Node {
                header: 4 << 8,
                left: l,
                right: r,
                i: ValueRef::from(0isize),
                j: ValueRef::from(0isize)
            });
            ValueRef::from(Shared::new(ptr as *mut PointyObject))
        }
    }

    fn empty_node(&mut self) -> ValueRef {
        self.new_node(ValueRef::from(0isize), ValueRef::from(0isize))
    }

    fn make_tree(&mut self, depth: usize) -> ValueRef {
        if depth == 0 {
            self.empty_node()
        } else {
            self.stack.push(ValueRef::from(depth as isize));
            let l = self.make_tree(depth - 1);
            self.stack.pop();

            self.stack.push(ValueRef::from(depth as isize));
            self.stack.push(ValueRef::from(l));
            let r = self.make_tree(depth - 1);
            self.stack.pop();
            self.stack.pop();

            self.new_node(l, r)
        }
    }

    fn populate(&mut self, mut depth: usize, node: Shared<Node>) {
        if depth > 0 {
            depth -= 1;

            unsafe {
                self.stack.push(ValueRef::from(depth as isize));
                self.stack.push(ValueRef::from(Shared::new(*node as *mut PointyObject)));
                (*node.as_mut_ptr()).left = self.empty_node();
                self.stack.pop();
                self.stack.pop();

                self.stack.push(ValueRef::from(depth as isize));
                self.stack.push(ValueRef::from(Shared::new(*node as *mut PointyObject)));
                (*node.as_mut_ptr()).right = self.empty_node();
                self.stack.pop();
                self.stack.pop();

                self.stack.push(ValueRef::from(depth as isize));
                self.stack.push(ValueRef::from(Shared::new(*node as *mut PointyObject)));
                self.populate(depth, transmute((**node).left.ptr().unwrap()));
                self.stack.pop();
                self.stack.pop();

                self.populate(depth, transmute((**node).right.ptr().unwrap()));
            }
        }
    }
}

impl Drop for GCBench {
    fn drop(&mut self) {
        unsafe { pcws_destroy_heap(self.heap) }
    }
}

#[bench]
fn populate(b: &mut Bencher) {
    let mut gcb = GCBench::new();
    let mut depth = GCBench::MIN_DEPTH;
    while depth <= GCBench::MAX_DEPTH {
        b.iter(|| {
            let temp_node = gcb.empty_node();
            gcb.populate(depth, unsafe { transmute(temp_node.ptr().unwrap()) });
        });
        depth += 2;
    }
}

#[bench]
fn make_tree(b: &mut Bencher) {
    let mut gcb = GCBench::new();
    let mut depth = GCBench::MIN_DEPTH;
    while depth <= GCBench::MAX_DEPTH {
        b.iter(|| gcb.make_tree(depth));
        depth += 2;
    }
}
