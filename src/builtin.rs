use alloc::rc::Rc;
use alloc::vec::Vec;

use crate::env::Env;
use crate::eval::EvalError;
use crate::value::Value;

pub fn default_env() -> Env {
    let mut env = Env::new();
    env.define(
        "cons",
        Value::Builtin {
            n_params: 2,
            func: Rc::new(cons),
        },
    );
    env
}

pub fn cons(args: Vec<Value>) -> Result<Value, EvalError> {
    let a = args.get(0).ok_or(EvalError::TooManyParams(2))?;
    let b = args.get(1).ok_or(EvalError::TooManyParams(1))?;
    Ok(a.cons(b))
}
