use alloc::string::String;

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
			"define" => eval_define(env, *cdr),
			"if" => eval_if(env, *cdr),
			"lambda" => eval_lambda(env, *cdr),
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
    let target = values
        .car()
        .ok_or(EvalError::SetMissingSymbol)?;
    match target {
	Value::Symbol(s) => {
	    let val = values
		.cadr()
		.ok_or(EvalError::SetMissingValue)?;
	    match env.set(&s, val) {
		SetResult::Undefined => Err(EvalError::SetUndefined(s)),
		SetResult::Success => Ok(Value::symbol("done")),
	    }
	}
	_ => Err(EvalError::CannotSetNonSymbol(target))
    }
}

fn eval_define(env: &mut Env, values: Value) -> EvalResult<Value> {
    let target = values
        .car()
        .ok_or(EvalError::DefineMissingTarget)?;
    match target {
	Value::Pair { car, cdr } => {
	    if let Value::Symbol(sym) = *car {
		let body = values
		    .cadr()
		    .ok_or(EvalError::DefineMissingBody)?;

		let lambda = Value::list(&[
		    Value::symbol("lambda"),
		    *cdr,
		    body,
		]);
		env.define(sym, lambda.clone());
		Ok(lambda)
	    } else {
		Err(EvalError::CannotDefineNonSymbol(*car))
	    }
	},
	Value::Symbol(sym) => {
	    let value = values
		.cadr()
		.ok_or(EvalError::DefineMissingValue)?;
	    env.define(&sym, value.clone());
	    Ok(value)
	},
	_ => Err(EvalError::CannotDefineNonSymbol(target)),
    }
}

fn eval_lambda(_: &mut Env, values: Value) -> EvalResult<Value> {
    Ok(Value::symbol("lambda").cons(&values))
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
	.ok_or(EvalError::IfMissingPredicate)?;
    let cons = values
	.cadr()
	.ok_or(EvalError::IfMissingConsequent)?;
    let alt = values.caddr();

    // Anything that's not 'false' counts as true in 'if'.
    if eval(env, pred)? != Value::False {
	eval(env, cons)
    } else if let Some(a) = alt {
	eval(env, a)
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
    DefineMissingTarget,
    CannotDefineNonSymbol(Value),
    DefineMissingValue,
    DefineMissingBody,
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
    use alloc::borrow::ToOwned;


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

    #[test]
    fn evaluating_lambda_works() {
	let mut e = Env::new();
	let lambda = Value::list(&[
	    Value::symbol("lambda"),
	    Value::list(&[
		Value::symbol("x"),
		Value::symbol("blah"),
	    ]),
	    Value::number(43),
	]);
	// Lambda should just be passed on until they are applied.
	assert_eq!(eval(&mut e, lambda.clone()).unwrap(), lambda);
    }

    #[test]
    fn evaluating_define_works() {
	let mut e = Env::new();
	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("define"),
		    Value::list(&[
			Value::symbol("second"),
			Value::symbol("x"),
			Value::symbol("y"),
		    ]),
		    Value::symbol("y"),
		]),
	    ).unwrap(),
	    Value::list(&[
		Value::symbol("lambda"),
		Value::list(&[
		    Value::symbol("x"),
		    Value::symbol("y"),
		]),
		Value::symbol("y"),
	    ]),
	);

	assert_eq!(
	    e.lookup("second").unwrap(),
	    Value::list(&[
		Value::symbol("lambda"),
		Value::list(&[
		    Value::symbol("x"),
		    Value::symbol("y"),
		]),
		Value::symbol("y")
	    ]),
	);

	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("define"),
		    Value::symbol("some-number"),
		    Value::number(54),
		]),
	    ).unwrap(),
	    Value::number(54),
	);
	assert_eq!(e.lookup("some-number"), Some(Value::number(54)));

	assert_eq!(
	    eval(&mut e, Value::list(&[Value::symbol("define")])).unwrap_err(),
	    EvalError::DefineMissingTarget,
	);

	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("define"),
		    Value::list(&[
			Value::number(1),
		    ]),
		])).unwrap_err(),
	    EvalError::CannotDefineNonSymbol(Value::number(1)),
	);

	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("define"),
		    Value::number(1),
		])).unwrap_err(),
	    EvalError::CannotDefineNonSymbol(Value::number(1)),
	);


	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("define"),
		    Value::symbol("blah"),
		])).unwrap_err(),
	    EvalError::DefineMissingValue,
	);

	assert_eq!(
	    eval(
		&mut e,
		Value::list(&[
		    Value::symbol("define"),
		    Value::list(&[
			Value::symbol("blah"),
		    ]),
		])).unwrap_err(),
	    EvalError::DefineMissingBody,
	);	
    }
}
