use alloc::string::String;
use alloc::borrow::ToOwned;

use crate::env::Env;
use crate::env::SetResult;
use crate::value::Value;

pub fn eval(env: &mut Env, value: Value) -> EvalResult<Value> {
    match value {
	Value::String(s) => Ok(Value::String(s)),
	Value::Number(n) => Ok(Value::Number(n)),
	Value::True => Ok(Value::True),
	Value::False => Ok(Value::False),
	Value::Symbol(s) => {
	    env.lookup(&s)
		.ok_or_else(|| EvalError::Undefined(s))
	},
	Value::Pair { car, cdr } => {
	    match *car {
		Value::Symbol(ref s) => {
		    match s.as_str() {
			"set!" => eval_set(env, *cdr),
			"define" => todo!(),
			"if" => eval_if(env, *cdr),
			"lambda" => todo!(),
			"begin" => eval_begin(env, *cdr),
			"cond" => todo!(),
			_ => {
			    apply(
				eval(env, *car)?,
				eval_list(env, *cdr)?
			    )
			},
		    }
		}
		_ => {
		    apply(
			eval(env, *car)?,
			eval_list(env, *cdr)?
		    )
		},
	    }
	},
	Value::Empty => Ok(Value::Empty),
    }
}

fn eval_set(env: &mut Env, values: Value) -> EvalResult<Value> {
    let sym = values
        .car()
        .map_err(|_| EvalError::SetMissingSymbol)?;
    match sym {
	Value::Symbol(s) => {
	    let val = values
		.cadr()
		.map_err(|_| EvalError::SetMissingValue)?;
	    match env.set(s, val.clone()) {
		SetResult::Undefined => Err(EvalError::SetUndefined(s.clone())),
		SetResult::Success => Ok(Value::symbol("done")),
	    }
	}
	_ => Err(EvalError::CannotSetNonSymbol(sym.clone()))
    }
}

fn eval_begin(env: &mut Env, mut values: Value) -> EvalResult<Value> {
    loop {
	match values {
	    Value::Pair { car, cdr } => {
		let res = eval(env, *car)?;
		if *cdr == Value::Empty {
		    return Ok(res);
		} else {
		    values = *cdr;
		}
	    },
	    // Default returned case of a '(begin)'.
	    Value::Empty => return Ok(Value::False),
	    _ => unreachable!("'begin' special forms must be inside a list"),
	}
    }
}

fn eval_if(env: &mut Env, values: Value) -> EvalResult<Value> {
    let pred = values
	.car()
	.map_err(|_| EvalError::IfMissingPredicate)?;
    let cons = values
	.cadr()
	.map_err(|_| EvalError::IfMissingConsequent)?;
    let alt = values
	.caddr()
	.ok();

    // Anything that's not 'false' counts as true in 'if'.
    if eval(env, pred.clone())? != Value::False {
	eval(env, cons.clone())
    } else if let Some(a) = alt {
	eval(env, a.clone())
    } else {
	// Default value if the predicate is false
	// and there is no alternative.
	Ok(Value::False)
    }
}

fn eval_list(env: &mut Env, value: Value) -> EvalResult<Value> {
    match value {
	Value::Pair { car, cdr } => {
	    Ok(
		eval(env, *car)?.cons(&eval_list(env, *cdr)?)
	    )
	}
	Value::Empty => Ok(Value::Empty),
	_ => Err(EvalError::NotAList(value)),
    }
}

fn apply(_procedure: Value, _arguments: Value) -> EvalResult<Value> {
    todo!()
}

type EvalResult<T> = Result<T, EvalError>;

// TODO: Add sources to the eval errors.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    Undefined(String),
    NotAList(Value),
    IfMissingPredicate,
    IfMissingConsequent,
    SetMissingSymbol,
    CannotSetNonSymbol(Value),
    SetMissingValue,
    SetUndefined(String),
}

pub fn default_env() -> Env {
    let e = Env::new();
    // TODO: Add `Value::True` and `Value::False`.
    // e.define("true", Value::True);
    // e.define("false", Value::False);
    // TODO: Add primitive procedures
    e
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn self_evaluating_values_self_evaluate() {
	let mut e = Env::new();
	assert_eq!(
	    eval(&mut e, Value::string("example string")).unwrap(),
	    Value::string("example string"),
	);
	assert_eq!(
	    eval(&mut e, Value::number(234)).unwrap(),
	    Value::number(234),
	);
	assert_eq!(
	    eval(&mut e, Value::True).unwrap(),
	    Value::True,
	);
	assert_eq!(
	    eval(&mut e, Value::False).unwrap(),
	    Value::False,
	);
    }

    #[test]
    fn evaluating_if_works() {
	let mut e = Env::new();
	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("if"),
		    Value::string("is am considered true"),
		    Value::string("true outcome"),
		    Value::string("false outcome"),
		]),
	    ).unwrap(),
	    Value::string("true outcome"),
	);
	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("if"),
		    Value::False,
		    Value::number(1),
		    Value::string("not true"),
		]),
	    ).unwrap(),
	    Value::string("not true"),
	);
    }

    // #[test]
    // fn evaluating_define_works() {
    // 	let mut e = Env::new();
    // 	assert_eq!(
    // 	    eval(
    // 		&mut e,
    // 		Value::list(&[
    // 		    Value::symbol("define"),
    // 		    Value::symbol("x"),
    // 		    Value::number(53),
    // 		]),
    // 	    ).unwrap(),
    // 	    Value::number(53),
    // 	);
    // 	assert_eq!(e.lookup("x"), Some(Value::number(53)));
    // }

    #[test]
    fn evaluating_set_works() {
	let mut e = Env::new();
	e.define("y", Value::number(43));
	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("set!"),
		    Value::symbol("y"),
		    Value::number(5),
		]),
	    ).unwrap(),
	    Value::symbol("done"),
	);
	assert_eq!(e.lookup("y"), Some(Value::number(5)));

	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("set!"),
		    Value::symbol("non-existent"),
		    Value::number(43),
		]),
	    ).unwrap_err(),
	    EvalError::SetUndefined("non-existent".to_owned()),
	);

	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("set!"),
		]),
	    ).unwrap_err(),
	    EvalError::SetMissingSymbol,
	);

	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("set!"),
		    Value::number(34.53),
		    Value::string("blah"),
		]),
	    ).unwrap_err(),
	    EvalError::CannotSetNonSymbol(Value::number(34.53)),
	);
    }

    #[test]
    fn evaluating_begin_works() {
	let mut e = Env::new();
	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("begin"),
		    Value::string("one"),
		    Value::string("two"),
		]),
	    ).unwrap(),
	    Value::string("two"),
	);

	e.define("x", Value::Empty);
	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("begin"),
		    Value::list(&[
			Value::symbol("set!"),
			Value::symbol("x"),
			Value::string("blah"),
		    ]),
		    Value::True
		]),
	    ).unwrap(),
	    Value::True,
	);
	assert_eq!(e.lookup("x"), Some(Value::string("blah")));

	assert_eq!(
	    eval(&mut e, Value::list(&[Value::symbol("begin")])).unwrap(),
	    Value::False,  // Default
	);
    }
}
