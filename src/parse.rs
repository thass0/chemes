use crate::value::*;
use crate::lex::*;

/// Turn a string of Scheme source code into a
/// Scheme value than can be evaluated.
pub fn parse<A: AsRef<str>>(src: A) -> Result<Value, ParseError> {
    let _tokens = lex(src.as_ref());
    Ok(Value::Empty)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParseError {
    LexError(LexError),
}
