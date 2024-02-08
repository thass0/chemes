use alloc::borrow::ToOwned;
use alloc::collections::BTreeMap;
use alloc::rc::Rc;
use alloc::string::String;
use core::cell::RefCell;

use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    bindings: BTreeMap<String, Value>,
    next: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            bindings: BTreeMap::new(),
            next: None,
        }
    }

    /// Add the given frame to the top of the current environment.
    pub fn extend(next: Rc<RefCell<Env>>) -> Self {
        let mut ext = Self::new();
        ext.next = Some(next);
        ext
    }

    /// Return the value behind `symbol` bound in the current
    /// environment or `None` if the symbol is not bound.
    pub fn lookup<A: AsRef<str>>(&self, symbol: A) -> Option<Value> {
        match self.bindings.get(symbol.as_ref()) {
            Some(v) => Some(v.clone()),
            None => match &self.next {
                None => None,
                Some(env) => env.borrow().lookup(symbol),
            },
        }
    }

    /// Change the value of `symbol` to `value`. If `symbol` is not bound
    /// to any value, `Undefined` is returned and the environment is not
    /// changed. If the symbol is bound, and its value is updated,
    /// `Success` is returned.
    pub fn set<A: AsRef<str>>(
        &mut self,
        symbol: A,
        value: Value,
    ) -> Result<(), UndefinedSymbolError> {
        if self.bindings.get(symbol.as_ref()).is_some() {
            self.bindings.insert(symbol.as_ref().to_owned(), value);
            Ok(())
        } else if let Some(next) = &self.next {
            next.borrow_mut().set(symbol, value)
        } else {
            Err(UndefinedSymbolError)
        }
    }

    /// Define `symbol` to `value` in the current environment. Overwrites
    /// the current value if `symbol` was defined before.
    pub fn define<A: AsRef<str>>(&mut self, symbol: A, value: Value) {
        self.bindings.insert(symbol.as_ref().to_owned(), value);
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UndefinedSymbolError;

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use alloc::boxed::Box;

    #[test]
    fn defining_works() {
        let mut e = Env::new();
        e.define("blah", Value::number(1));
        assert_eq!(e.lookup("blah"), Some(Value::number(1)));
    }

    #[test]
    fn setting_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        e.borrow_mut().define("blah", Value::number(0));
        assert_eq!(e.borrow().lookup("blah"), Some(Value::number(0)));

        let mut ext = Env::extend(e.clone());
        ext.set("blah", Value::string("I was set")).unwrap();
        assert_eq!(e.borrow().lookup("blah"), Some(Value::string("I was set")));
    }

    #[test]
    fn defining_lambdas_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        let lambda = Value::Lambda {
            params: Box::new(Value::list(&[Value::symbol("x")])),
            body: Box::new(Value::symbol("x")),
            env: e.clone(),
        };
        e.borrow_mut().define("id", lambda.clone());
    }

    #[test]
    fn set_and_define_work_in_extended_envs() {
        let e = Rc::new(RefCell::new(Env::new()));
        e.borrow_mut()
            .define("set-var", Value::string("I'm not set yet!"));
        assert_eq!(
            e.borrow().lookup("set-var"),
            Some(Value::string("I'm not set yet!"))
        );
        {
            let mut ext = Env::extend(e.clone());
            ext.define("defined-var", Value::string("I'm defined"));
            ext.set("set-var", Value::string("I'm set")).unwrap();
            assert_eq!(
                ext.lookup("defined-var"),
                Some(Value::string("I'm defined"))
            );
            assert_eq!(ext.lookup("set-var"), Some(Value::string("I'm set")));
        }
        // Changes made by 'set' propagate; those made by 'define' don't!
        assert_eq!(e.borrow().lookup("defined_var"), None);
        assert_eq!(e.borrow().lookup("set-var"), Some(Value::string("I'm set")));
    }

    #[test]
    fn stacking_extended_envs_works() {
        let e = Rc::new(RefCell::new(Env::new()));
        e.borrow_mut().define("outer", Value::number(0));
        assert_eq!(e.borrow().lookup("outer"), Some(Value::number(0)));
        {
            let ext = Rc::new(RefCell::new(Env::extend(e.clone())));
            ext.borrow_mut().define("middle", Value::number(1));
            ext.borrow_mut()
                .set("outer", Value::string("from middle"))
                .unwrap();
            assert_eq!(ext.borrow().lookup("middle"), Some(Value::number(1)));
            assert_eq!(
                ext.borrow().lookup("outer"),
                Some(Value::string("from middle"))
            );
            {
                let mut ext = Env::extend(ext.clone());
                ext.define("inner", Value::number(2));
                ext.set("middle", Value::string("from inner")).unwrap();
                assert_eq!(ext.lookup("inner"), Some(Value::number(2)));
                assert_eq!(ext.lookup("middle"), Some(Value::string("from inner")));
            }
            assert_eq!(ext.borrow().lookup("inner"), None);
            assert_eq!(
                ext.borrow().lookup("middle"),
                Some(Value::string("from inner"))
            );
        }
        assert_eq!(e.borrow().lookup("middle"), None);
        assert_eq!(
            e.borrow().lookup("outer"),
            Some(Value::string("from middle"))
        );
    }
}
