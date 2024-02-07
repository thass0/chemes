use alloc::string::String;

use crate::env::Env;
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
			"set!" => todo!(),
			"define" => todo!(),
			"if" => eval_if(env, *cdr),
			"lambda" => todo!(),
			"begin" => todo!(),
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

type EvalResult<T> = Result<T, EvalError>;

// TODO: Add sources to the eval errors.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    Undefined(String),
    NotAList(Value),
    IfMissingPredicate,
    IfMissingConsequent,
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

    if eval(env, pred.clone())?.is_truthy() {
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
		Value::if_(
		    &Value::False,
		    &Value::number(1),
		    &Value::string("not true")
		),
	    ).unwrap(),
	    Value::string("not true"),
	);
    }
}
