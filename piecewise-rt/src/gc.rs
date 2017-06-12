
#[cfg(test)]
mod tests {
    use core::nonzero::NonZero;
    use std::mem::size_of;
    use std::ptr;
    use std::ptr::Shared;
    use std::mem::transmute;
    use test::Bencher;

    use mark_n_sweep::MSHeap;
    use object_model::{GCRef, Object, Header};
    use allocator::Allocator;

    #[repr(C)]
    struct Node {
        header: Header,
        left: GCRef,
        right: GCRef,
        i: GCRef,
        j: GCRef
    }

    impl From<Shared<Node>> for GCRef {
        fn from(node: Shared<Node>) -> Self {
            GCRef::from(unsafe { transmute::<_, Shared<Object>>(node) })
        }
    }

    struct GCBench {
        heap: MSHeap,
        stack: Vec<GCRef>
    }

    impl GCBench {
        const MIN_DEPTH: usize = 4;
        const MAX_DEPTH: usize = 16;

        fn new() -> Self {
            GCBench {
                heap: MSHeap::new(),
                stack: Vec::new()
            }
        }

        fn new_node(&mut self, l: GCRef, r: GCRef) -> GCRef {
            unsafe {
                let ptr = *self.heap.allocate(NonZero::new(1),
                                              NonZero::new(size_of::<Node>() / size_of::<GCRef>()))
                               .or_else(|| {
                                   self.heap.mark_ref(l);
                                   self.heap.mark_ref(r);
                                   for root in self.stack.iter() {
                                       self.heap.mark_ref(*root);
                                   }
                                   self.heap.collect();

                                   self.heap.allocate(NonZero::new(1),
                                                      NonZero::new(size_of::<Node>() /
                                                                   size_of::<GCRef>()))
                               })
                               .expect("Heap exhausted!") as *mut Node;
                ptr::write(ptr, Node {
                    header: Header::new(4),
                    left: l,
                    right: r,
                    i: GCRef::from(0isize),
                    j: GCRef::from(0isize)
                });
                GCRef::from(Shared::new(ptr as *mut Object))
            }
        }

        fn empty_node(&mut self) -> GCRef {
            self.new_node(GCRef::from(0isize), GCRef::from(0isize))
        }

        fn make_tree(&mut self, depth: usize) -> GCRef {
            if depth == 0 {
                self.empty_node()
            } else {
                self.stack.push(GCRef::from(depth as isize));
                let l = self.make_tree(depth - 1);
                self.stack.pop();

                self.stack.push(GCRef::from(depth as isize));
                self.stack.push(GCRef::from(l));
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
                    self.stack.push(GCRef::from(depth as isize));
                    self.stack.push(GCRef::from(node));
                    (*node.as_mut_ptr()).left = self.empty_node();
                    self.stack.pop();
                    self.stack.pop();

                    self.stack.push(GCRef::from(depth as isize));
                    self.stack.push(GCRef::from(node));
                    (*node.as_mut_ptr()).right = self.empty_node();
                    self.stack.pop();
                    self.stack.pop();

                    self.stack.push(GCRef::from(depth as isize));
                    self.stack.push(GCRef::from(node));
                    self.populate(depth, transmute((**node).left.ptr().unwrap()));
                    self.stack.pop();
                    self.stack.pop();

                    self.populate(depth, transmute((**node).right.ptr().unwrap()));
                }
            }
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
}
