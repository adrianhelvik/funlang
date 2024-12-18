use std::io::Write;

use crate::context::FunCtx;
use crate::{builtins::*, FuncCall};
use crate::{interpreter::InterpreterResult, BuiltinFunc, LocExpr, Variable};

pub type BuiltinFn<W> = fn(&FunCtx<W>, &FuncCall) -> InterpreterResult;

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinName {
    ListPush,
    ListLen,
    Print,
    Println,
    Eq,
    Modulo,
    Add,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Type,
    Map,
    Or,
    Not,
    Lazy,
    Str,
    Sub,
    Mul,
}

pub fn lookup_global_builtins(var: &Variable) -> Option<LocExpr> {
    use BuiltinName as B;

    let def = |name: B| {
        var.loc.builtin_func(BuiltinFunc {
            name,
            loc: var.loc.clone(),
            this: var.loc.null().boxed(),
        })
    };

    Some(match var.ident.as_ref() {
        "print" => def(B::Print),
        "println" => def(B::Println),
        "eq" => def(B::Eq),
        "modulo" => def(B::Modulo),
        "lt" => def(B::Lt),
        "lte" => def(B::Lte),
        "gt" => def(B::Gt),
        "gte" => def(B::Gte),
        "and" => def(B::And),
        "add" => def(B::Add),
        "+" => def(B::Add),
        "type" => def(B::Type),
        "Map" => def(B::Map),
        "or" => def(B::Or),
        "not" => def(B::Not),
        "lazy" => def(B::Lazy),
        "str" => def(B::Str),
        "sub" => def(B::Sub),
        "-" => def(B::Sub),
        "mul" => def(B::Mul),
        "*" => def(B::Mul),
        _ => return None,
    })
}

impl BuiltinFunc {
    pub fn as_string(&self) -> String {
        use BuiltinName as B;
        match self.name {
            B::ListPush => "list.push",
            B::ListLen => "list.len",
            B::Print => "print",
            B::Println => "println",
            B::Eq => "eq",
            B::Modulo => "modulo",
            B::Add => "add",
            B::Lt => "lt",
            B::Lte => "lte",
            B::Gt => "gt",
            B::Gte => "gte",
            B::And => "and",
            B::Type => "type",
            B::Map => "Map",
            B::Or => "or",
            B::Not => "not",
            B::Lazy => "lazy",
            B::Str => "str",
            B::Sub => "sub",
            B::Mul => "*",
        }
        .to_string()
    }

    pub fn call<W: Write>(&self, ctx: &FunCtx<W>, func_call: &FuncCall) -> InterpreterResult {
        use BuiltinName as B;

        let call = |f: BuiltinFn<W>| f(ctx, &func_call.with_this(*self.this.clone()));

        call(match self.name {
            B::ListPush => fun_list_push,
            B::ListLen => fun_list_len,
            B::Print => fun_print,
            B::Println => fun_println,
            B::Eq => fun_eq,
            B::Modulo => fun_modulo,
            B::Add => fun_add,
            B::Lt => fun_lt,
            B::Lte => fun_lte,
            B::Gt => fun_gt,
            B::Gte => fun_gte,
            B::And => fun_and,
            B::Type => fun_type,
            B::Map => fun_create_map,
            B::Or => fun_or,
            B::Not => fun_not,
            B::Lazy => fun_lazy,
            B::Str => fun_str,
            B::Sub => fun_sub,
            B::Mul => fun_mul,
        })
    }
}
