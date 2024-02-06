#![no_std]
#![feature(error_in_core)]

extern crate alloc;

mod lex;

mod parse;
pub use parse::*;

mod value;
mod env;
