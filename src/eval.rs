use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;
use core::cell::RefCell;
use core::cmp::Ordering;
use core::iter::zip;

use crate::env::Env;
use crate::syntax;
use crate::value::Value;

/// Evaluate a single value. This is the primary function that drives
/// the evaluation process of a Scheme program.
///
/// To apply functions, it calls itself recursively and then applies
/// the function body to the function's arguments.
///
/// `eval` modifies the environment if 'set!' or 'define' are called.
///
/// It also transforms derived expressions (syntactic sugar) into
/// primitive values to evaluate them.
pub fn eval(env: Rc<RefCell<Env>>, value: Value) -> EvalResult<Value> {
    match value {
        s @ Value::String(_) => Ok(s),
        n @ Value::Number(_) => Ok(n),
        l @ Value::Lambda { .. } => Ok(l),
        b @ Value::Builtin { .. } => Ok(b),
        Value::True => Ok(Value::True),
        Value::False => Ok(Value::False),
        Value::Symbol(s) => env
            .borrow()
            .lookup(&s)
            .ok_or_else(|| EvalError::Undefined(s)),
        Value::Pair(car, cdr) => match *car {
            Value::Symbol(ref s) => match s.as_str() {
                "set!" => eval_set(env, *cdr),
                "define" => eval_define(env, *cdr),
                "if" => eval_if(env, *cdr),
                "lambda" => eval_lambda(env, *cdr),
                "begin" => eval_begin(env, *cdr),
                "quote" => eval_quote(*cdr),
                "cond" => eval(env, syntax::cond(*cdr)?),
                _ => apply(eval(env.clone(), *car)?, eval_sequence(env, *cdr)?),
            },
            _ => apply(eval(env.clone(), *car)?, eval_sequence(env, *cdr)?),
        },
        Value::Empty => Err(EvalError::LiteralEmptyList),
    }
}

fn eval_set(env: Rc<RefCell<Env>>, values: Value) -> EvalResult<Value> {
    let target = values.car().ok_or(EvalError::SetMissingSymbol)?;
    match target {
        Value::Symbol(s) => {
            let val = values.cadr().ok_or(EvalError::SetMissingValue)?;
            match env.borrow_mut().set(&s, val) {
                Err(_) => Err(EvalError::SetUndefined(s)),
                Ok(_) => Ok(Value::symbol("done")),
            }
        }
        _ => Err(EvalError::CannotSetNonSymbol(target)),
    }
}

fn eval_define(env: Rc<RefCell<Env>>, values: Value) -> EvalResult<Value> {
    let target = values.car().ok_or(EvalError::DefineMissingTarget)?;
    match target {
        Value::Pair(car, cdr) => {
            if let Value::Symbol(sym) = *car {
                let body = values.cadr().ok_or(EvalError::DefineMissingBody)?;
                let lambda = Value::Lambda {
                    params: Box::new(*cdr.clone()),
                    body: Box::new(body.clone()),
                    env: env.clone(),
                };
                env.borrow_mut().define(&sym, lambda.clone());
                Ok(lambda)
            } else {
                Err(EvalError::CannotDefineNonSymbol(*car))
            }
        }
        Value::Symbol(sym) => {
            let value = values.cadr().ok_or(EvalError::DefineMissingValue)?;
            env.borrow_mut().define(&sym, value.clone());
            Ok(value)
        }
        _ => Err(EvalError::CannotDefineNonSymbol(target)),
    }
}

fn eval_lambda(env: Rc<RefCell<Env>>, values: Value) -> EvalResult<Value> {
    let params = values.car().ok_or(EvalError::LambdaMissingParams)?;
    let body = values.cadr().ok_or(EvalError::LambdaMissingBody)?;
    Ok(Value::Lambda {
        params: Box::new(params),
        body: Box::new(body),
        env: env.clone(),
    })
}

fn eval_begin(env: Rc<RefCell<Env>>, mut values: Value) -> EvalResult<Value> {
    loop {
        match values {
            Value::Pair(car, cdr) => {
                let res = eval(env.clone(), *car)?;
                if *cdr == Value::Empty {
                    return Ok(res);
                } else {
                    values = *cdr;
                }
            }
            // Default returned case of a '(begin)'.
            Value::Empty => return Ok(Value::False),
            _ => unreachable!("'begin' special forms must be inside a list"),
        }
    }
}

fn eval_quote(values: Value) -> EvalResult<Value> {
    let q = values.car().ok_or(EvalError::QuoteMissingValue)?;
    let e = values.cdr().ok_or(EvalError::QuoteMissingValue)?;
    if e == Value::Empty {
        Ok(q)
    } else {
        Err(EvalError::QuoteTooManyValues)
    }
}

fn eval_if(env: Rc<RefCell<Env>>, values: Value) -> EvalResult<Value> {
    let pred = values.car().ok_or(EvalError::IfMissingPredicate)?;
    let cons = values.cadr().ok_or(EvalError::IfMissingConsequent)?;
    let alt = values.caddr();

    // Anything that's not 'false' counts as true in 'if'.
    if eval(env.clone(), pred)? != Value::False {
        eval(env, cons)
    } else if let Some(a) = alt {
        eval(env, a)
    } else {
        // Default value if the predicate is false
        // and there is no alternative.
        Ok(Value::False)
    }
}

fn eval_sequence(env: Rc<RefCell<Env>>, value: Value) -> EvalResult<Value> {
    match value {
        Value::Pair(car, cdr) => Ok(eval(env.clone(), *car)?.cons(&eval_sequence(env, *cdr)?)),
        Value::Empty => Ok(Value::Empty),
        _ => Err(EvalError::NotAList(value)),
    }
}

pub type EvalResult<T> = Result<T, EvalError>;

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
    LambdaMissingParams,
    LambdaMissingBody,
    TypeError, // TODO: Add more details.
    QuoteMissingValue,
    QuoteTooManyValues,
    LiteralEmptyList,

    // For apply
    TooManyArgs(usize),
    TooManyParams(usize),
    NonSymbolParam(Value),
    CannotApplyNonLambda(Value),

    // Syntax error
    Syntax(syntax::SyntaxError),
}

impl From<syntax::SyntaxError> for EvalError {
    fn from(s: syntax::SyntaxError) -> Self {
        Self::Syntax(s)
    }
}

fn check_params(n_params: usize, n_args: usize) -> EvalResult<()> {
    match n_params.cmp(&n_args) {
        Ordering::Greater => {
            return Err(EvalError::TooManyParams(n_params - n_args));
        }
        Ordering::Less => {
            return Err(EvalError::TooManyArgs(n_args - n_params));
        }
        Ordering::Equal => {}
    }
    Ok(())
}

fn apply(lambda: Value, args: Value) -> EvalResult<Value> {
    let args = args.iter().collect::<Vec<Value>>();
    match lambda {
        Value::Builtin(func) => func(args),
        Value::Lambda { params, body, env } => {
            let params = params.iter().collect::<Vec<Value>>();
            check_params(params.len(), args.len())?;

            let syms = {
                let mut syms = Vec::with_capacity(params.len());
                for param in params {
                    if let Value::Symbol(s) = param {
                        syms.push(s);
                    } else {
                        return Err(EvalError::NonSymbolParam(param));
                    }
                }
                syms
            };

            let mut ext = Env::extend(env);
            for (sym, arg) in zip(syms, args) {
                ext.define(sym, arg);
            }
            eval(Rc::new(RefCell::new(ext)), *body)
        }
        _ => Err(EvalError::CannotApplyNonLambda(lambda)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::borrow::ToOwned;

    #[test]
    fn self_evaluating_values_self_evaluate() {
        let e = Rc::new(RefCell::new(Env::new()));
        assert_eq!(
            eval(e.clone(), Value::string("example string")).unwrap(),
            Value::string("example string"),
        );
        assert_eq!(
            eval(e.clone(), Value::number(234)).unwrap(),
            Value::number(234),
        );
        assert_eq!(eval(e.clone(), Value::True).unwrap(), Value::True,);
        assert_eq!(eval(e.clone(), Value::False).unwrap(), Value::False,);
    }

    #[test]
    fn evaluating_if_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("if"),
                    Value::string("is am considered true"),
                    Value::string("true outcome"),
                    Value::string("false outcome"),
                ]),
            )
            .unwrap(),
            Value::string("true outcome"),
        );
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("if"),
                    Value::False,
                    Value::number(1),
                    Value::string("not true"),
                ]),
            )
            .unwrap(),
            Value::string("not true"),
        );
    }

    #[test]
    fn evaluating_set_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        e.borrow_mut().define("y", Value::number(43));
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("set!"), Value::symbol("y"), Value::number(5),]),
            )
            .unwrap(),
            Value::symbol("done"),
        );
        assert_eq!(e.borrow().lookup("y"), Some(Value::number(5)));

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("set!"),
                    Value::symbol("non-existent"),
                    Value::number(43),
                ]),
            )
            .unwrap_err(),
            EvalError::SetUndefined("non-existent".to_owned()),
        );

        assert_eq!(
            eval(e.clone(), Value::list(&[Value::symbol("set!"),]),).unwrap_err(),
            EvalError::SetMissingSymbol,
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("set!"),
                    Value::number(34.53),
                    Value::string("blah"),
                ]),
            )
            .unwrap_err(),
            EvalError::CannotSetNonSymbol(Value::number(34.53)),
        );
    }

    #[test]
    fn evaluating_begin_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("begin"),
                    Value::string("one"),
                    Value::string("two"),
                ]),
            )
            .unwrap(),
            Value::string("two"),
        );

        e.borrow_mut().define("x", Value::Empty);
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("begin"),
                    Value::list(&[
                        Value::symbol("set!"),
                        Value::symbol("x"),
                        Value::string("blah"),
                    ]),
                    Value::True
                ]),
            )
            .unwrap(),
            Value::True,
        );
        assert_eq!(e.borrow().lookup("x"), Some(Value::string("blah")));

        assert_eq!(
            eval(e.clone(), Value::list(&[Value::symbol("begin")])).unwrap(),
            Value::False, // Default
        );
    }

    #[test]
    fn evaluating_raw_lambda_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        let raw = Value::list(&[
            Value::symbol("lambda"),
            Value::list(&[Value::symbol("x"), Value::symbol("blah")]),
            Value::number(43),
        ]);
        let lambda = Value::Lambda {
            params: Box::new(Value::list(&[Value::symbol("x"), Value::symbol("blah")])),
            body: Box::new(Value::number(43)),
            env: e.clone(),
        };
        assert_eq!(eval(e.clone(), raw).unwrap(), lambda);
    }

    #[test]
    fn evaluting_lambda_value_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        let lambda = Value::Lambda {
            params: Box::new(Value::list(&[Value::symbol("example")])),
            body: Box::new(Value::string("return this")),
            env: e.clone(),
        };
        assert_eq!(eval(e.clone(), lambda.clone()).unwrap(), lambda);
    }

    #[test]
    fn evaluating_define_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("define"),
                    Value::list(&[
                        Value::symbol("second"),
                        Value::symbol("x"),
                        Value::symbol("y"),
                    ]),
                    Value::symbol("y"),
                ]),
            )
            .unwrap(),
            Value::Lambda {
                params: Box::new(Value::list(&[Value::symbol("x"), Value::symbol("y")])),
                body: Box::new(Value::symbol("y")),
                env: e.clone(),
            }
        );

        assert_eq!(
            e.borrow().lookup("second").unwrap(),
            Value::Lambda {
                params: Box::new(Value::list(&[Value::symbol("x"), Value::symbol("y")])),
                body: Box::new(Value::symbol("y")),
                env: e.clone(),
            }
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("define"),
                    Value::symbol("x"),
                    Value::number(53),
                ]),
            )
            .unwrap(),
            Value::number(53),
        );
        assert_eq!(e.borrow().lookup("x"), Some(Value::number(53)));

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("define"),
                    Value::symbol("some-number"),
                    Value::number(54),
                ]),
            )
            .unwrap(),
            Value::number(54),
        );
        assert_eq!(e.borrow().lookup("some-number"), Some(Value::number(54)));
    }

    #[test]
    fn eval_catched_invalid_defines() {
        let e = Rc::new(RefCell::new(Env::new()));
        assert_eq!(
            eval(e.clone(), Value::list(&[Value::symbol("define")])).unwrap_err(),
            EvalError::DefineMissingTarget,
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("define"), Value::list(&[Value::number(1),]),])
            )
            .unwrap_err(),
            EvalError::CannotDefineNonSymbol(Value::number(1)),
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("define"), Value::number(1),])
            )
            .unwrap_err(),
            EvalError::CannotDefineNonSymbol(Value::number(1)),
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("define"), Value::symbol("blah"),])
            )
            .unwrap_err(),
            EvalError::DefineMissingValue,
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("define"),
                    Value::list(&[Value::symbol("blah"),]),
                ])
            )
            .unwrap_err(),
            EvalError::DefineMissingBody,
        );
    }

    #[test]
    fn applying_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        e.borrow_mut().define(
            "id",
            Value::Lambda {
                params: Box::new(Value::list(&[Value::symbol("x")])),
                body: Box::new(Value::symbol("x")),
                env: e.clone(),
            },
        );
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("id"), Value::number((3, 4)),]),
            )
            .unwrap(),
            Value::number((3, 4)),
        );

        e.borrow_mut().define(
            "apply1",
            Value::Lambda {
                params: Box::new(Value::list(&[Value::symbol("f"), Value::symbol("arg")])),
                body: Box::new(Value::list(&[Value::symbol("f"), Value::symbol("arg")])),
                env: e.clone(),
            },
        );
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("apply1"),
                    Value::symbol("id"),
                    Value::string("Everyone just passed me around ):"),
                ]),
            )
            .unwrap(),
            Value::string("Everyone just passed me around ):"),
        );
    }

    #[test]
    fn evaluating_builtin_procedures_works() {
        use crate::builtin::default_env;
        let e = Rc::new(RefCell::new(default_env()));
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("cons"),
                    Value::string("Cars drive on roads"),
                    Value::string("A cdr certainly doesn't drive!"),
                ]),
            )
            .unwrap(),
            Value::Pair(
                Box::new(Value::string("Cars drive on roads")),
                Box::new(Value::string("A cdr certainly doesn't drive!")),
            ),
        );
    }

    #[test]
    fn evaluating_quotes_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[
                    Value::symbol("quote"),
                    Value::list(&[Value::number(1), Value::symbol("blah"),]),
                ]),
            )
            .unwrap(),
            Value::list(&[Value::number(1), Value::symbol("blah")]),
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("quote"), Value::symbol("x"),])
            )
            .unwrap(),
            Value::symbol("x"),
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("quote"), Value::list(&[]),]),
            )
            .unwrap(),
            Value::Empty,
        );

        assert_eq!(
            eval(e.clone(), Value::list(&[Value::symbol("quote")]),).unwrap_err(),
            EvalError::QuoteMissingValue,
        );

        assert_eq!(
            eval(
                e.clone(),
                Value::list(&[Value::symbol("quote"), Value::number(1), Value::number(2),]),
            )
            .unwrap_err(),
            EvalError::QuoteTooManyValues,
        );
    }

    #[test]
    fn evaluating_cond_works() {
        {
            // Run the initial path.
            let e = Rc::new(RefCell::new(Env::new()));
            e.borrow_mut().define("x", Value::number(1));
            e.borrow_mut().define("y", Value::number(2));
            e.borrow_mut().define("z", Value::number(3));
            assert_eq!(
                eval(
                    e.clone(),
                    Value::list(&[
                        Value::symbol("cond"),
                        Value::list(&[
                            Value::symbol("x"),
                            Value::list(&[
                                Value::symbol("set!"),
                                Value::symbol("y"),
                                Value::number(4),
                            ]),
                            Value::symbol("z"),
                        ]),
                    ])
                )
                .unwrap(),
                Value::number(3),
            );
            assert_eq!(e.borrow().lookup("y").unwrap(), Value::number(4));
        }
        {
            // Run the 'else' clause.
            let e = Rc::new(RefCell::new(Env::new()));
            e.borrow_mut().define("x", Value::False);
            e.borrow_mut().define("y", Value::False);
            assert_eq!(
                eval(
                    e.clone(),
                    Value::list(&[
                        Value::symbol("cond"),
                        Value::list(&[Value::symbol("x"), Value::number(1),]),
                        Value::list(&[
                            Value::symbol("else"),
                            Value::list(&[
                                Value::symbol("set!"),
                                Value::symbol("y"),
                                Value::string("so exciting"),
                            ]),
                            Value::number(4324),
                        ]),
                    ])
                )
                .unwrap(),
                Value::number(4324),
            );
            assert_eq!(
                e.borrow().lookup("y").unwrap(),
                Value::string("so exciting")
            );
        }
        {
            // Fall through the first few cases.
            let e = Rc::new(RefCell::new(Env::new()));
            e.borrow_mut().define("x", Value::False);
            e.borrow_mut().define("y", Value::False);
            e.borrow_mut().define("z", Value::number(1));
            assert_eq!(
                eval(
                    e.clone(),
                    Value::list(&[
                        Value::symbol("cond"),
                        Value::list(&[Value::symbol("x"), Value::number(1)]),
                        Value::list(&[Value::symbol("y"), Value::number(2)]),
                        Value::list(&[Value::symbol("z"), Value::number(3)]),
                    ]),
                )
                .unwrap(),
                Value::number(3),
            );
            assert_eq!(
                eval(
                    e.clone(),
                    Value::list(&[
                        Value::symbol("cond"),
                        Value::list(&[Value::symbol("x"), Value::number(1)]),
                        Value::list(&[Value::symbol("y"), Value::number(2)]),
                    ]),
                )
                .unwrap(),
                Value::False,
            );
        }
        {
            // Edge cases
            let e = Rc::new(RefCell::new(Env::new()));
            assert_eq!(
                eval(e.clone(), Value::list(&[Value::symbol("cond")]),).unwrap(),
                Value::False,
            );
            assert_eq!(
                eval(
                    e.clone(),
                    Value::list(&[Value::symbol("cond"), Value::list(&[Value::number(0)]),]),
                )
                .unwrap(),
                Value::False,
            );
            assert_eq!(
                eval(
                    e.clone(),
                    Value::list(&[Value::symbol("cond"), Value::list(&[Value::symbol("else")]),]),
                )
                .unwrap(),
                Value::False
            );
        }
    }

    #[test]
    fn reject_unquoted_empty_list() {
        let e = Rc::new(RefCell::new(Env::new()));
        assert_eq!(
            eval(e.clone(), Value::Empty,).unwrap_err(),
            EvalError::LiteralEmptyList,
        );

        assert_eq!(
            eval(e.clone(), Value::list(&[Value::Empty,]),).unwrap_err(),
            EvalError::LiteralEmptyList,
        );
    }
}
