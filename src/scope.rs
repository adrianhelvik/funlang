use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{Expression, Loc, LocError};

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

    pub fn assign(&self, ident: String, expr: Expression) -> Expression {
        self.values
            .borrow_mut()
            .insert(ident.to_string(), expr.clone());
        expr
    }

    pub fn reassign(
        &self,
        loc: &Loc,
        ident: String,
        expr: Expression,
    ) -> Result<Expression, LocError> {
        let val = {
            let values = self.values.borrow();
            values.get(&ident).cloned()
        };
        match val {
            Some(_) => {
                self.values
                    .borrow_mut()
                    .insert(ident.to_string(), expr.clone());
                Ok(expr)
            }
            None => {
                let parent = self
                    .parent
                    .as_ref()
                    .ok_or_else(|| {
                        loc.error(&format!(
                            "Attempted to reassign undeclared variable '{}'",
                            ident.clone()
                        ))
                    })?
                    .borrow();
                parent.reassign(&loc, ident, expr)
            }
        }
    }

    pub fn has(&self, ident: String) -> bool {
        self.values.borrow().contains_key(&ident)
    }

    pub fn get(&self, ident: &str) -> Option<Expression> {
        match self.values.borrow().get(ident) {
            Some(expr) => Some(expr.clone()),
            None => {
                let parent = self.parent.as_ref()?.borrow();
                parent.get(ident).clone()
            }
        }
    }
}

