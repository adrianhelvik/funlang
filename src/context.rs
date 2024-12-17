use std::{cell::RefCell, io::Write, rc::Rc};

use crate::scope::Scope;

pub struct FunContext<W: Write> {
    pub scope: Rc<Scope>,
    pub output: Rc<RefCell<W>>,
    pub filename: String,
}
