use colorful::Color;
use colorful::Colorful;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::context::FunCtx;
use crate::interpreter::eval_expr_and_call_returned_block;
use crate::scope::Scope;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub ident: Identifier,
    pub arg: Box<Expression>,
    pub this: Box<Expression>,
}

impl FuncCall {
    pub fn with_this(&self, this: Expression) -> FuncCall {
        return FuncCall {
            ident: self.ident.clone(),
            arg: self.arg.clone(),
            this: Box::new(this),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ident: String,
    pub loc: Loc,
}

impl Variable {
    pub fn new(token: Token) -> Self {
        Variable {
            ident: token.value,
            loc: token.loc,
        }
    }

    pub fn not_defined_err(&self) -> Result<Expression, LocError> {
        Err(LocError {
            loc: self.loc.clone(),
            message: format!("Variable '{}' is not defined", self.ident),
        })
    }

    pub fn error(&self, message: &str) -> LocError {
        self.loc.error(message)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    String(String),
    Int(i64),
    FuncCall(FuncCall),
    Touple(Vec<Expression>),
    VarDecl(Box<VarDecl>),
    ReAssignment(Box<ReAssignment>),
    Variable(Variable),
    FuncExpr(FuncExpr),
    Null,
    Return(Box<Expression>),
    Closure(Box<Closure>),
    IfExpr(Box<IfExpr>),
    Bool(bool),
    Block(FuncCall),
    Map(Rc<RefCell<HashMap<String, Expression>>>),
    ForExpr(ForExpr),
    WhileExpr(WhileExpr),
    List(List),
    Access(Access),
    ImportExpr(ImportExpr),
    Lazy(Box<LazyExpression>),
    BuiltinFuncCall(Builtin),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeLiteral {
    pub start: Expression,
    pub end: Expression
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinName {
    ListPush,
    ListLen,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Builtin {
    pub loc: Loc,
    pub name: BuiltinName,
    pub this: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LazyExpression {
    pub expr: Expression,
    pub scope: Rc<Scope>,
}

impl LazyExpression {
    pub fn eval<W: Write>(
        &self,
        ctx: &FunCtx<W>
    ) -> Result<Expression, LocError> {
        eval_expr_and_call_returned_block(&self.expr, ctx)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportExpr {
    pub loc: Loc,
    pub source: String,
    pub symbols: Vec<Token>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Access {
    pub loc: Loc,
    pub target: Box<Expression>,
    pub key: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub items: Rc<RefCell<Vec<Expression>>>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Expression,
    pub then_expr: Expression,
    pub else_expr: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForExpr {
    pub identifier: Variable,
    pub range: Box<RangeLiteral>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    pub condition: Box<Expression>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub func_expr: FuncExpr,
    pub scope: Rc<Scope>,
}

// Implement PartialEq for Closure
impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.func_expr == other.func_expr && Rc::ptr_eq(&self.scope, &other.scope)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub ident: String,
    pub expr: Expression,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReAssignment {
    pub ident: Identifier,
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub accessors: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncExpr {
    pub args: Vec<String>,
    pub expressions: Vec<Expression>,
}


#[derive(Debug, PartialEq, Clone)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
}

impl Loc {
    pub fn new(line: usize, column: usize) -> Self {
        Loc { line, column }
    }

    pub fn error(&self, message: &str) -> LocError {
        LocError {
            loc: self.clone(),
            message: message.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub value: String,
    pub loc: Loc,
}

impl Token {
    pub fn from(value: &str, loc: Loc) -> Self {
        Token {
            value: value.to_string(),
            loc,
        }
    }

    pub fn new(value: String, loc: Loc) -> Self {
        Token { value, loc }
    }

    pub fn to_variable(&self) -> Variable {
        Variable {
            ident: self.value.clone(),
            loc: self.loc.clone(),
        }
    }
}

pub type Tokens = Vec<Token>;

#[derive(Debug, PartialEq, Clone)]
pub struct LocError {
    pub message: String,
    pub loc: Loc,
    //pub ignorable: bool,
}

#[derive(Debug, PartialEq)]
pub struct SyntaxError {
    pub message: String,
}

impl SyntaxError {
    pub fn new(message: String) -> Self {
        SyntaxError { message }
    }

    pub fn from(message: &str) -> Self {
        SyntaxError {
            message: message.to_string(),
        }
    }

    pub fn generate(err: LocError, source: String) -> Self {
        let mut lines = source.split("\n");
        let colored_prefix = "Error:".to_string();
        let uncolored_prefix = " ".to_string();
        let line = lines.nth(err.loc.line - 1).unwrap();
        let mut message_line = String::new();
        for _ in 0..(err.loc.column - 1 + colored_prefix.len() + uncolored_prefix.len()) {
            message_line.push(' ');
        }
        message_line += format!(
            "{}",
            format!("^ [{}:{}] ", err.loc.line, err.loc.column).color(Color::Cyan)
        )
        .as_str();
        SyntaxError {
            message: format!(
                "{}{}{}\n{}{}",
                colored_prefix.white().bg_red(),
                uncolored_prefix,
                line.bold(),
                message_line,
                err.message.gradient(Color::Red)
            ),
        }
    }

    // TODO: Remove duplication
    pub fn generate_plain(err: LocError, source: String) -> Self {
        let mut lines = source.split("\n");
        let colored_prefix = "Error:".to_string();
        let uncolored_prefix = " ".to_string();
        let line = lines.nth(err.loc.line - 1).unwrap();
        let mut message_line = String::new();
        for _ in 0..(err.loc.column - 1 + colored_prefix.len() + uncolored_prefix.len()) {
            message_line.push(' ');
        }
        message_line += format!("{}", format!("^ [{}:{}] ", err.loc.line, err.loc.column)).as_str();
        SyntaxError {
            message: format!(
                "{}{}{}\n{}{}",
                colored_prefix, uncolored_prefix, line, message_line, err.message
            ),
        }
    }
}

