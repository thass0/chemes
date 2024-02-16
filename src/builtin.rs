use alloc::rc::Rc;
use alloc::vec::Vec;

use crate::env::Env;
use crate::eval::{EvalError, EvalResult};
use crate::value::Value;

pub fn default_env() -> Env {
    let mut env = Env::new();
    env.define("cons", Value::Builtin(Rc::new(cons)));
    env.define("car", Value::Builtin(Rc::new(car)));
    env.define("cdr", Value::Builtin(Rc::new(cdr)));
    env.define("list", Value::Builtin(Rc::new(list)));
    env
}

macro_rules! n_params {
    ($args:expr, $n:expr) => {{
        use core::cmp::Ordering;
        match $args.len().cmp(&$n) {
            Ordering::Less => Err(EvalError::TooManyParams($n - $args.len())),
            Ordering::Greater => Err(EvalError::TooManyArgs($args.len() - $n)),
            Ordering::Equal => Ok(()),
        }
    }};
}

pub fn cons(args: Vec<Value>) -> EvalResult<Value> {
    n_params!(args, 2)?;
    let a = &args[0];
    let b = &args[1];
    Ok(a.cons(b))
}

pub fn car(args: Vec<Value>) -> EvalResult<Value> {
    n_params!(args, 1)?;
    let a = &args[0];
    a.car().ok_or(EvalError::TypeError)
}

pub fn cdr(args: Vec<Value>) -> EvalResult<Value> {
    n_params!(args, 1)?;
    let a = &args[0];
    a.cdr().ok_or(EvalError::TypeError)
}

pub fn list(args: Vec<Value>) -> EvalResult<Value> {
    Ok(Value::list(&args))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! eval_default_eq {
        ($a:expr, $b:expr $(,)?) => {
            let e = alloc::rc::Rc::new(core::cell::RefCell::new($crate::builtin::default_env()));
            assert_eq!($crate::eval::eval(e.clone(), $a), $b);
        };
    }

    #[test]
    fn cons_works() {
        eval_default_eq!(
            Value::list(&[Value::symbol("cons"), Value::string("a"), Value::number(14),]),
            Ok(Value::string("a").cons(&Value::number(14)))
        );
        eval_default_eq!(
            Value::list(&[
                Value::symbol("cons"),
                Value::string("a"),
                Value::number(14),
                Value::string("too may of 'em")
            ]),
            Err(EvalError::TooManyArgs(1)),
        );
        eval_default_eq!(
            Value::list(&[
                Value::symbol("cons"),
                Value::string("a"),
                Value::number(14),
                Value::string("too may of 'em"),
                Value::string("even more"),
            ]),
            Err(EvalError::TooManyArgs(2)),
        );
        eval_default_eq!(
            Value::list(&[Value::symbol("cons"), Value::string("a"),]),
            Err(EvalError::TooManyParams(1)),
        );
        eval_default_eq!(
            Value::list(&[Value::symbol("cons")]),
            Err(EvalError::TooManyParams(2)),
        );
    }

    #[test]
    fn car_works() {
        eval_default_eq!(
            Value::list(&[
                Value::symbol("car"),
                Value::list(&[
                    Value::symbol("quote"),
                    Value::list(&[Value::symbol("a"), Value::symbol("b"),]),
                ]),
            ]),
            Ok(Value::symbol("a")),
        );
    }

    #[test]
    fn cdr_works() {
        eval_default_eq!(
            Value::list(&[
                Value::symbol("cdr"),
                Value::quote(Value::list(&[Value::symbol("a"), Value::symbol("b"),]),),
            ]),
            Ok(Value::list(&[Value::symbol("b")])),
        );
    }

    #[test]
    fn list_works() {
        eval_default_eq!(
            Value::list(&[
                Value::symbol("list"),
                Value::string("I'm part of a list"),
                Value::number(2343.4),
                Value::quote(Value::symbol("x")),
            ]),
            Ok(Value::list(&[
                Value::string("I'm part of a list"),
                Value::number(2343.4),
                Value::symbol("x"),
            ]),),
        );
    }
}
