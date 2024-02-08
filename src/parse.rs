use alloc::vec::Vec;

use crate::lex::*;
use crate::value::*;

/// Parse a string into a sequence of Scheme values that can be evaluated.
pub fn parse<A: AsRef<str>>(src: A) -> ParseResult<Vec<Value>> {
    let tokens = lex(src.as_ref());
    parse_tokens(&mut tokens.into_iter())
}

/// Drive turning all tokens into a sequence of values
/// until the token stream is empty or an error occurs.
fn parse_tokens<I>(tokens: &mut I) -> ParseResult<Vec<Value>>
where
    I: Iterator<Item = Token>,
{
    let mut seq = Vec::new();
    loop {
        let val = to_value(tokens, 0);
        match val {
            Cont(v) => seq.push(v),
            Eof => return Ok(seq),
            CloseParen => return Result::Err(ParseError::UnmatchedRParen),
            Err(e) => return Result::Err(e),
        }
    }
}

/// Recursively parse tokens into a single value.
fn to_value<I>(tokens: &mut I, level: i32) -> ToValueState
where
    I: Iterator<Item = Token>,
{
    if let Some(tok) = tokens.next() {
        match &tok.kind {
            TokenKind::String(s) => Cont(Value::string(s)),
            TokenKind::Number(n) => Cont(Value::number(*n)),
            TokenKind::Symbol(s) => match s.as_str() {
                "true" | "#t" => Cont(Value::True),
                "false" | "#f" => Cont(Value::False),
                _ => Cont(Value::symbol(s)),
            },
            TokenKind::LParen => {
                // Recursively call this function until an `RParen` at
                // the same level as this `LParen` returns an empty list.

                let mut seq = Vec::new();
                loop {
                    match to_value(tokens, level + 1) {
                        Cont(v) => seq.push(v),
                        CloseParen => {
                            if seq.is_empty() {
                                return Err(ParseError::UnquotedEmptyList);
                            } else {
                                return Cont(Value::list(&seq));
                            }
                        }
                        Eof => return Err(ParseError::UnmatchedLParen),
                        Err(e) => return Err(e),
                    }
                }
            }
            TokenKind::RParen => {
                if level > 0 {
                    CloseParen
                } else {
                    Err(ParseError::UnmatchedRParen)
                }
            }
            TokenKind::Eof => Eof,
        }
    } else {
        unreachable!("token streams must end with EOF");
    }
}

/// Parse state of the `to_value` function.
#[derive(Debug, Clone)]
enum ToValueState {
    Err(ParseError),
    Cont(Value),
    Eof,
    CloseParen,
}

use ToValueState::*;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    UnmatchedLParen,
    UnmatchedRParen,
    EmptyInput,
    UnquotedEmptyList,
}

#[cfg(test)]
mod tests {
    use super::*;

    use alloc::vec;

    #[test]
    fn accept_example_values() {
        assert_eq!(parse("blah").unwrap(), vec![Value::symbol("blah")]);
        assert_eq!(parse("\"blah\"").unwrap(), vec![Value::string("blah")]);
        assert_eq!(parse("31").unwrap(), vec![Value::number(31)]);
        assert_eq!(
            parse("(foo? bar. baz!)").unwrap(),
            vec![Value::symbol("foo?")
                .cons(&Value::symbol("bar.").cons(&Value::symbol("baz!").cons(&Value::Empty)))],
        );
        assert_eq!(
            parse("(true #f false #t)").unwrap(),
            vec![Value::list(&[
                Value::True,
                Value::False,
                Value::False,
                Value::True,
            ])],
        );
        assert_eq!(
            parse("(foo (bar \"x\") ((y z) 3))").unwrap(),
            vec![Value::list(&[
                Value::symbol("foo"),
                Value::list(&[Value::symbol("bar"), Value::string("x"),]),
                Value::list(&[
                    Value::list(&[Value::symbol("y"), Value::symbol("z"),]),
                    Value::number(3),
                ]),
            ])],
        );
    }

    #[test]
    fn accept_multiple_values() {
        assert_eq!(
            parse("(I) cannot (think (of good)) examples!").unwrap(),
            vec![
                Value::list(&[Value::symbol("I")]),
                Value::symbol("cannot"),
                Value::list(&[
                    Value::symbol("think"),
                    Value::list(&[Value::symbol("of"), Value::symbol("good"),]),
                ]),
                Value::symbol("examples!"),
            ],
        )
    }

    #[test]
    fn reject_unmatche_left_parenthesis() {
        assert_eq!(parse("((what?!)").unwrap_err(), ParseError::UnmatchedLParen,);

        assert_eq!(
            parse("(what-comes-after-next? -- nothing").unwrap_err(),
            ParseError::UnmatchedLParen,
        );
    }

    #[test]
    fn reject_unmatched_right_parenthesis() {
        assert_eq!(
            parse("Did I forget something?)").unwrap_err(),
            ParseError::UnmatchedRParen,
        );

        assert_eq!(
            parse("(this-is-wrong: ))").unwrap_err(),
            ParseError::UnmatchedRParen,
        )
    }

    #[test]
    fn reject_unquoted_empty_list() {
        assert_eq!(parse("()").unwrap_err(), ParseError::UnquotedEmptyList,);

        assert_eq!(
            parse("(cannot use the empty list (anywhere ()))").unwrap_err(),
            ParseError::UnquotedEmptyList,
        );
    }
}
