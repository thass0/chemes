use alloc::borrow::*;
use alloc::boxed::Box;
use alloc::fmt;
use alloc::fmt::Formatter;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;
use core::cell::RefCell;

use crate::env::Env;
use crate::eval::EvalError;

/// A Scheme value. It represents both the AST of a program and runtime
/// values since Scheme is a homoiconic (i.e. "code as data") language.
#[derive(Clone)]
pub enum Value {
    String(String),
    Number(Number),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Empty,
    True,
    False,
    Lambda {
        params: Box<Value>,
        body: Box<Value>,
        env: Rc<RefCell<Env>>,
    },
    Builtin(Rc<dyn Fn(Vec<Value>) -> Result<Value, EvalError>>),
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
        Self::Pair(Box::new(self.clone()), Box::new(other.clone()))
    }

    /// Construct a list of values (as pairs).
    ///
    /// # Example
    /// ```
    /// use chemes::Value;
    /// assert_eq!(Value::list(&[]), Value::Empty);
    /// ```
    pub fn list(elems: &[Self]) -> Self {
        let mut p = Value::Empty;
        for elem in elems.iter().rev() {
            p = elem.cons(&p);
        }
        p
    }

    /// Construct a quoted value.
    ///
    /// # Example
    /// ```
    /// use chemes::Value;
    /// assert_eq!(
    ///   Value::quote(Value::symbol("x")),
    ///   Value::list(&[
    ///     Value::symbol("quote"),
    ///     Value::symbol("x"),
    ///   ])
    /// );
    /// ```
    pub fn quote(v: Value) -> Self {
        Value::list(&[Value::symbol("quote"), v.clone()])
    }

    /// Get the 'car' of a pair value.
    pub fn car(&self) -> Option<Value> {
        match self {
            Value::Pair(car, _) => Some(*car.clone()),
            _ => None,
        }
    }

    /// Get the 'cdr' of a pair value.
    pub fn cdr(&self) -> Option<Value> {
        match self {
            Value::Pair(_, cdr) => Some(*cdr.clone()),
            _ => None,
        }
    }

    /// Get the 'cadr' of a pair value.
    pub fn cadr(&self) -> Option<Value> {
        self.cdr()?.car()
    }

    /// Get the 'caddr' of a pair value.
    pub fn caddr(&self) -> Option<Value> {
        self.cdr()?.cdr()?.car()
    }

    /// Turn this value into an iterator.
    pub fn iter(self) -> impl core::iter::Iterator<Item = Self> {
        self
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Number(n) => write!(f, "{n}"),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Lambda { params, .. } => {
                write!(f, "<procedure> (<?>")?;
                for param in params.clone().iter() {
                    write!(f, " {param}")?;
                }
                Ok(())
            }
            Value::Builtin(_) => {
                write!(f, "<builtin procedure>")
            }
            Value::True => write!(f, "true"),
            Value::False => write!(f, "false"),
            Value::Pair(car, cdr) => {
                // TODO: Detect lists and print them properly.
                write!(f, "( {car} . {cdr})")
            }
            Value::Empty => write!(f, "()"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "String({s:?})"),
            Value::Number(n) => write!(f, "Number({n:?})"),
            Value::Symbol(s) => write!(f, "Symbol({s})"),
            Value::Lambda { params, body, env } => {
                write!(
                    f,
                    "Lambda {{ params: {params:?}, body: {body:?}, env: {:?} }}",
                    env.as_ptr()
                )
            }
            Value::Builtin(_) => write!(f, "Builtin(_)"),
            Value::True => write!(f, "True"),
            Value::False => write!(f, "False"),
            Value::Pair(car, cdr) => {
                write!(f, "Pair {{ car: {car:?}, cdr: {cdr:?} }}")
            }
            Value::Empty => write!(f, "Empty"),
        }
    }
}

impl core::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => *s1 == *s2,
            (Value::Number(n1), Value::Number(n2)) => *n1 == *n2,
            (Value::Symbol(s1), Value::Symbol(s2)) => *s1 == *s2,
            (Value::Pair(car1, cdr1), Value::Pair(car2, cdr2)) => *car1 == *car2 && *cdr1 == *cdr2,
            (Value::Empty, Value::Empty) => true,
            (Value::True, Value::True) => true,
            (Value::False, Value::False) => true,
            (
                Value::Lambda {
                    params: params1,
                    body: body1,
                    env: env1,
                },
                Value::Lambda {
                    params: params2,
                    body: body2,
                    env: env2,
                },
            ) => *params1 == *params2 && *body1 == *body2 && env1.as_ptr() == env2.as_ptr(),
            _ => false,
        }
    }
}

impl core::iter::Iterator for Value {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.clone() {
            Value::Pair(car, cdr) => {
                *self = *cdr;
                Some(*car)
            }
            Value::Empty => None,
            v => {
                *self = Value::Empty;
                Some(v)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(i32),
    Rat {
        p: i32, // Numerator
        q: u32, // Denominator
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construct_values() {
        let p = Value::string("blah").cons(&Value::number(42));
        assert_eq!(p.car(), Some(Value::string("blah")));
        assert_eq!(p.cdr(), Some(Value::number(42)));
        assert_eq!(
            Value::list(&[Value::string("a"), Value::number(4), Value::Empty]),
            Value::Pair(
                Box::new(Value::string("a")),
                Box::new(Value::Pair(
                    Box::new(Value::number(4)),
                    Box::new(Value::Pair(Box::new(Value::Empty), Box::new(Value::Empty),)),
                )),
            ),
        );
        assert_eq!(p.cdr().unwrap().car(), None);
        assert_eq!(p.cdr().unwrap().cdr(), None);
        assert_eq!(
            Value::True.cons(&Value::False),
            Value::Pair(Box::new(Value::True), Box::new(Value::False)),
        );
    }
}
