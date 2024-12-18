use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{interpreter::InterpreterResult, setup_builtins::lookup_global_builtins, Expression, LocError, LocExpr, Variable};

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub values: Box<RefCell<HashMap<String, Expression>>>,
    pub parent: Option<RefCell<Rc<Scope>>>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            values: Box::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn create(parent: &Rc<Scope>) -> Self {
        Scope {
            values: Box::new(RefCell::new(HashMap::new())),
            parent: Some(Rc::clone(parent).into()),
        }
    }

    pub fn assign(&self, ident: String, expr: LocExpr) -> LocExpr {
        self.values
            .borrow_mut()
            .insert(ident.to_string(), expr.expr.clone());
        expr
    }

    pub fn reassign(
        &self,
        var: &Variable,
        expr: LocExpr,
    ) -> Result<LocExpr, LocError> {
        let val = {
            let values = self.values.borrow();
            values.get(&var.ident).cloned()
        };
        match val {
            Some(_) => {
                self.values
                    .borrow_mut()
                    .insert(var.ident.to_string(), expr.expr.clone());
                Ok(expr)
            }
            None => {
                let parent = self
                    .parent
                    .as_ref()
                    .ok_or_else(|| {
                        var.loc.error(&format!(
                            "Attempted to reassign undeclared variable '{}'",
                            var.ident.clone()
                        ))
                    })?
                    .borrow();
                parent.reassign(var, expr)
            }
        }
    }

    pub fn has(&self, ident: String) -> bool {
        self.values.borrow().contains_key(&ident)
    }

    pub fn get(&self, var: &Variable) -> InterpreterResult {
        if let Some(expr) = lookup_global_builtins(var) {
            return Ok(expr);
        } else {
            self.get_non_global(var)
        }
    }

    fn get_non_global(&self, var: &Variable) -> InterpreterResult {
        match self.values.borrow().get(&var.ident) {
            Some(expr) => Ok(var.loc.wrap(expr.clone())),
            None => {
                if let Some(parent) = self.parent.as_ref() {
                    let parent = parent.borrow();
                    parent.get(var).clone()
                } else {
                    Err(var.loc.error(&format!("Attempted to access undefined variable '{}'", var.ident)))
                }
            }
        }
    }
}

