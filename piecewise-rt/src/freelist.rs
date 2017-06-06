use std::ptr;
use std::mem::transmute;

// TODO: use intrusive-collections crate instead

/// Interface for FreeList value-nodes.
pub trait Node {
    fn next(&self) -> *mut Self;
    fn set_next(&mut self, new_next: *mut Self);

    fn prev(&self) -> *mut Self;
    fn set_prev(&mut self, new_prev: *mut Self);
}

/// An intrusive list
pub struct FreeList<T: Node> {
    head: *mut T
}

impl<T: Node> FreeList<T> {
    /// Create an empty freelist.
    pub fn new() -> Self {
        FreeList {
            head: ptr::null_mut()
        }
    }

    /// Is the list empty?
    pub fn is_empty(&self) -> bool { self.head.is_null() }

    /// Add a value-node to the front.
    pub fn push_front(&mut self, new_node: &mut T) {
        let old_head = self.head;
        let new_node_ptr: *mut T = unsafe { transmute(new_node) };
        let new_node = unsafe { &mut *new_node_ptr };

        self.head = new_node_ptr;

        new_node.set_next(old_head);
        new_node.set_prev(ptr::null_mut());

        if !old_head.is_null() {
            unsafe { (*old_head).set_prev(new_node_ptr); }
        }
    }

    /// Remove the first value-node. Returns null if list was empty.
    pub fn pop_front(&mut self) -> *mut T {
        let old_head = self.head;

        unsafe {
            if !old_head.is_null() {
                self.head = (*old_head).next();
                (*self.head).set_prev(ptr::null_mut());
            }
        }

        old_head
    }

    /// Remove a value-node from anywhere in the list.
    pub fn remove(&mut self, node: &mut T) {
        if !node.prev().is_null() {
            unsafe { (*node.prev()).set_next(node.next()); }
        } else {
            self.head = node.next();
        }

        if !node.next().is_null() {
            unsafe { (*node.next()).set_prev(node.prev()); }
        }
    }

    /// Create iterator.
    pub fn iter(&self) -> Iter<T> {
        Iter { node: self.head }
    }
}

pub struct Iter<T: Node> {
    node: *mut T
}

impl<T: Node> Iterator for Iter<T> {
    type Item = *mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.node.is_null() {
            let res = self.node;
            unsafe { self.node = (*self.node).next(); }
            Some(res)
        } else {
            None
        }
    }
}
