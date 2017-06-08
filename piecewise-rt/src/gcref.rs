#[derive(Clone, Copy)]
struct Header(usize);

impl Header {
    fn obj_len(self) -> usize { self.0 } // FIXME
}

#[derive(Clone, Copy)]
pub struct GCRef(*mut GCRef);

impl GCRef {
    pub fn is_marked(self) -> bool { self.mark_byte() != 0 }

    pub fn mark(self) { self.set_mark_byte(1) }

    pub fn len(self) -> usize {
        unsafe { (*(self.0 as *mut Header)).obj_len() } // FIXME
    }

    pub fn fields(self) -> Fields {
        Fields {
            oref: self,
            index: 0
        }
    }

    unsafe fn unchecked_read(self, i: usize) -> GCRef {
        *(self.0).offset(i as isize + 1)
    }

    fn mark_byte_addr(self) -> *mut u8 {
        unimplemented!()
    }

    fn mark_byte(self) -> u8 { unsafe { *self.mark_byte_addr() } }

    fn set_mark_byte(self, n: u8) { unsafe { *self.mark_byte_addr() = n; } }
}

pub struct Fields {
    oref: GCRef,
    index: usize
}

impl Iterator for Fields {
    type Item = GCRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.oref.len() {
            let fref = unsafe { self.oref.unchecked_read(self.index) };
            self.index += 1;
            Some(fref)
        } else {
            None
        }
    }
}
