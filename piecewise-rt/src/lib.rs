#![feature(nonzero, unique, shared, associated_consts, test)]

extern crate core;
extern crate nix;
#[macro_use]
extern crate intrusive_collections;


#[cfg(test)]
extern crate test;
#[cfg(test)]
#[macro_use]
extern crate quickcheck;

pub mod util;
pub mod object_model;
pub mod allocator;
pub mod freelist;
pub mod arena;
pub mod arena_arr;
pub mod block;
pub mod block_arr;
pub mod mark_n_sweep;
pub mod gc;
