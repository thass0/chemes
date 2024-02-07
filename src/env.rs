use alloc::string::String;
use alloc::collections::BTreeMap;
use alloc::collections::btree_map::{Keys, Values};
use alloc::vec::Vec;
use alloc::vec;
use alloc::borrow::ToOwned;

use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    frames: Vec<Frame>,
}

#[derive(Debug, Clone, PartialEq)]
struct Frame {
    vars: BTreeMap<String, Value>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SetResult {
    Success,
    Undefined,
}

impl Env {
    pub fn new() -> Self {
	Env { frames: vec![Frame::new()] }
    }

    // fn enclosing_environment(&self) -> Option<Self> {
    // 	if self.frames.len() == 1 {
    // 	    None
    // 	} else {
    // 	    let mut frames = self.frames.clone();
    // 	    frames.pop();
    // 	    Some(Env { frames })
    // 	}
    // }

    /// Add a new frame on top of the current environment.
    pub fn push(&mut self) {
	self.frames.push(Frame::new());
    }

    /// Delete the top frame from the current environment.
    pub fn pop(&mut self) {
	self.frames.pop();
    }

    /// Return the value behind `symbol` bound in the current
    /// environment or `None` if the symbol is not bound.
    pub fn lookup<A: AsRef<str>>(&self, symbol: A) -> Option<Value> {
	for frame in self.frames.iter().rev() {
	    match frame.lookup(&symbol) {
		Some(v) => return Some(v),
		None => {},
	    }
	}
	None
    }

    /// Change the value of `symbol` to `value`. If `symbol` is not bound
    /// to any value, `Undefined` is returned and the environment is not
    /// changed. If the symbol is bound, and its value is updated,
    /// `Success` is returned.
    pub fn set<A: AsRef<str>>(&mut self, symbol: A, value: Value) -> SetResult {
	for frame in self.frames.iter_mut().rev() {
	    if frame.lookup(&symbol).is_some() {
		frame.bind(symbol.as_ref().to_owned(), value);
		return SetResult::Success;
	    }
	}
	SetResult::Undefined
    }

    /// Define `symbol` to `value` in the current environment. Overwrites
    /// the current value if `symbol` was defined before.
    pub fn define<A: AsRef<str>>(&mut self, symbol: A, value: Value) {
	match self.frames.last_mut() {
	    Some(frame) => {
		frame.bind(symbol.as_ref().to_owned(), value);
	    },
	    None => unreachable!("an environment must have at least on frame"),
	}
    }
}

impl Frame {
    fn new() -> Frame {
	Frame { vars: BTreeMap::new() }
    }

    fn from<I: Iterator<Item = (String, Value)>>(vars: I) -> Frame {
	let mut frame = Frame::new();
	for (symbol, value) in vars {
	    frame.bind(symbol, value);
	}
	frame
    }

    fn symbols(&self) -> Keys<'_, String, Value> {
	self.vars.keys()
    }

    fn values(&self) -> Values<'_, String, Value> {
	self.vars.values()
    }

    fn bind(&mut self, symbol: String, value: Value) {
	self.vars.insert(symbol, value);
    }

    fn lookup<A: AsRef<str>>(&self, symbol: A) -> Option<Value> {
	self.vars.get(symbol.as_ref()).cloned()
    }
}
