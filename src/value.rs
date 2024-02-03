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
    Number(Number),
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
    pub fn number<N: Into<Number>>(n: N) -> Value {
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

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i32),
    Rat {
	p: i32,  // Numerator
	q: u32,  // Denominator
    },
    Real(f64),
}

impl From<i32> for Number {
    fn from(i: i32) -> Self {
	Self::Int(i)
    }
}

impl From<(i32, u32)> for Number {
    fn from(z: (i32, u32)) -> Self {
	Self::Rat { p: z.0, q: z.1 }
    }
}

impl From<f64> for Number {
    fn from(r: f64) -> Self {
	Self::Real(r)
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
	match self {
	    Self::Int(i) => write!(f, "{i}"),
	    Self::Rat { p, q } => write!(f, "{p}/{q}"),
	    Self::Real(r) => write!(f, "{r}"),
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
