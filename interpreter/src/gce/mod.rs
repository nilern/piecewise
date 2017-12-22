// HACK: these are pub just for rustdoc
pub mod util;
pub mod descriptor;
pub mod layout;
pub mod arena;
pub mod block;
pub mod mark_n_sweep;

use std::ptr::Shared;

use gce::layout::GSize;

pub trait Object {
    fn gsize(&self) -> GSize;
}

pub trait ObjectRef: Copy {
    type Obj: Object;
    type PORef: PointyObjectRef;

    fn ptr(self) -> Option<Shared<Self::Obj>>;
    fn pointy_ref(self) -> Option<Self::PORef>;
}

pub trait PointyObjectRef {
    type ORef;

    fn obj_refs<'a>(self) -> &'a mut[Self::ORef];
}
