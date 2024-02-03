/// A Scheme value.

use alloc::string::String;
use alloc::boxed::Box;
use alloc::borrow::*;
use alloc::fmt;
use alloc::fmt::Formatter;

/// A Scheme value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Symbol(String),
    Pair {
	car: Box<Value>,
	cdr: Box<Value>,
    },
    Empty,
}

impl Value {
    /// Construct a string value.
    pub fn string<A: AsRef<str>>(s: A) -> Value {
	Value::String(s.as_ref().to_owned())
    }

    /// Construct a number value.
    pub fn number<N: Into<f64>>(n: N) -> Value {
	Value::Number(n.into())
    }

    /// Construct a symbol value.
    pub fn symbol<A: AsRef<str>>(s: A) -> Value {
	Value::Symbol(s.as_ref().to_owned())
    }

    /// Construct a pair value.
    pub fn cons(&self, other: &Self) -> Self {
	Self::Pair {
	    car: Box::new(self.clone()),
	    cdr: Box::new(other.clone()),
	}
    }

    /// Get the 'car' of a pair value.
    pub fn car<'a>(&'a self) -> TypeResult<'a, &'a Value> {
	match self {
	    Value::Pair { car, cdr: _ } => Ok(car.as_ref()),
	    _ => Err(TypeError::CarExpectsPair(self)),
	}
    }

    /// Get the 'cdr' of a pair value.
    pub fn cdr<'a>(&'a self) -> TypeResult<'a, &'a Value> {
	match self {
	    Value::Pair { car: _, cdr } => Ok(cdr.as_ref()),
	    _ => Err(TypeError::CdrExpectsPair(self)),
	}
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
	match self {
	    Value::String(s) => write!(f, "\"{s}\""),
	    Value::Number(n) => write!(f, "{n}"),
	    Value::Symbol(s) => write!(f, "{s}"),
	    // TODO: Detect and display lists properly
	    Value::Pair { car, cdr } =>
		write!(f, "( {car} . {cdr})"),
	    Value::Empty => write!(f, "()"),
	}
    }
}

pub type TypeResult<'a, T> = Result<T, TypeError<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError<'a> {
    CarExpectsPair(&'a Value),
    CdrExpectsPair(&'a Value),
}

impl<'a> core::error::Error for TypeError<'a> {}

impl<'a> fmt::Display for TypeError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
	match self {
	    TypeError::CarExpectsPair(v) =>
		write!(
		    f,
		    "'car' expects to receive a pair. Instead it got: {}",
		    v
		),
	    TypeError::CdrExpectsPair(v) =>
		write!(
		    f,
		    "'cdr' expects to receive a pair. Instead it got: {}",
		    v
		),
	}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construct_values() {
	let p = Value::string("blah").cons(&Value::number(42));
	assert_eq!(p.car(), Ok(&Value::string("blah")));
	assert_eq!(p.cdr(), Ok(&Value::number(42)));
	assert_eq!(
	    p.cdr().unwrap().car(),
	    Err(TypeError::CarExpectsPair(&Value::number(42)))
	);
	assert_eq!(
	    p.cdr().unwrap().cdr(),
	    Err(TypeError::CdrExpectsPair(&Value::number(42)))
	);
    }
}
