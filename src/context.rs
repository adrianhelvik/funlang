use std::{cell::RefCell, io::Write, rc::Rc};

use crate::scope::Scope;

pub struct FunCtx<W: Write> {
    pub scope: Rc<Scope>,
    pub output: Rc<RefCell<W>>,
    pub filename: String,
}

impl<W: Write> FunCtx<W> {
    pub fn write(&self, text: &str) {
        write!(self.output.borrow_mut(), "{}", &text).unwrap();
    }

    pub fn writeln(&self, text: &str) {
        writeln!(self.output.borrow_mut(), "{}", &text).unwrap();
    }
}
