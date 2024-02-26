//! Syntactic sugar that's added on top of the normal
//! evaluation logic. The functions in this module
//! transform values of so-called 'derived expressions'
//! (aka syntax sugar) into primitive special forms.

use crate::value::Value;

/// Transform a 'cond' form into a set of nested 'if' forms.
/// `value` is expected to be '(cdr (cond ...))'.
pub fn cond(value: Value) -> Result<Value, SyntaxError> {
    match value {
        Value::Pair(first, rest) => {
            let pred = first.car().ok_or(SyntaxError::CondNotAList)?;
            let actions = first.cdr().ok_or(SyntaxError::CondNotAList)?;

            if let Value::Symbol(ref s) = pred {
                if "else" == s {
                    if *rest == Value::Empty {
                        return Ok(make_begin(actions));
                    } else {
                        return Err(SyntaxError::CondElseIsntLast);
                    }
                }
            }

            Ok(make_if(pred, make_begin(actions), cond(*rest)?))
        }
        Value::Empty => Ok(Value::False),
        // If this call is reached, it means that `cond` was to
        // transform a value that should not be evaluated as a
        // `cond` form.
        _ => unreachable!("cond can only be applied in a list"),
    }
}

fn make_if(pred: Value, cons: Value, alt: Value) -> Value {
    Value::list(&[Value::symbol("if"), pred, cons, alt])
}

fn make_begin(seq: Value) -> Value {
    Value::symbol("begin").cons(&seq)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SyntaxError {
    CondNotAList,
    CondElseIsntLast,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cond_syntax_works() {
        let cond_src = r#"
(cond ((eq? a b) blah)
      (x y)
      (else
       (foo)
       (bar)))
"#;
        let expanded_src = r#"
(if (eq? a b)
    (begin blah)
    (if x
	(begin y)
	(begin
	  (foo)
	  (bar))))
"#;
        let cnd = parse_one(cond_src).cdr().unwrap();
        let expanded = parse_one(expanded_src);
        assert_eq!(cond(cnd), Ok(expanded));

        // If no action is specified, no code will run.
        assert_eq!(
            cond(parse_one("(cond (blah))").cdr().unwrap()),
            Ok(make_if(
                Value::symbol("blah"),
                make_begin(Value::Empty),
                Value::False,
            ),),
        );

        assert_eq!(cond(parse_one("(cond)").cdr().unwrap()), Ok(Value::False),);
    }

    #[test]
    fn cond_reject_invalid_syntax() {
        assert_eq!(
            cond(parse_one("(cond (else blah) (x y))").cdr().unwrap()),
            Err(SyntaxError::CondElseIsntLast),
        );
        assert_eq!(
            cond(parse_one("(cond blah)").cdr().unwrap()),
            Err(SyntaxError::CondNotAList),
        );
    }

    fn parse_one(src: &str) -> Value {
        crate::parse(src).unwrap()[0].clone()
    }
}
