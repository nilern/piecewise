#![feature(associated_consts)]

extern crate nix;
#[macro_use]
extern crate intrusive_collections;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

mod util;
pub mod gcref;
pub mod descriptor;
pub mod arena;
pub mod arena_arr;
pub mod block;
pub mod block_arr;
pub mod mark_n_sweep;
