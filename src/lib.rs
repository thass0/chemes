#![no_std]
#![feature(error_in_core)]

extern crate alloc;

mod lex;

mod parse;
pub use parse::*;

mod env;
pub use env::*;

mod syntax;

mod eval;
pub use eval::*;

mod value;
pub use value::*;

mod builtin;
pub use builtin::default_env;
