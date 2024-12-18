use colorful::Color;
use colorful::Colorful;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::context::FunCtx;
use crate::interpreter::eval_expr_and_call_returned_block;
use crate::scope::Scope;
use crate::setup_builtins::BuiltinName;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expressions: Vec<LocExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub target: Box<LocExpr>,
    pub arg: Box<LocExpr>,
    pub this: Box<LocExpr>,
}

impl FuncCall {
    pub fn with_this(&self, this: LocExpr) -> FuncCall {
        return FuncCall {
            target: self.target.clone(),
            arg: self.arg.clone(),
            this: Box::new(this),
        }
    }

    pub fn loc(&self) -> &Loc {
        &self.arg.loc
    }

    pub fn expr(&self, expr: Expression) -> LocExpr {
        self.loc().wrap(expr)
    }

    pub fn error(&self, message: &str) -> LocError {
        self.loc().error(message)
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

    pub fn not_defined_err(&self) -> Result<LocExpr, LocError> {
        Err(LocError {
            loc: self.loc.clone(),
            message: format!("Variable '{}' is not defined", self.ident),
        })
    }

    pub fn error(&self, message: &str) -> LocError {
        self.loc.error(message)
    }

    pub fn expr(self) -> LocExpr {
        let loc = self.loc.clone();
        loc.wrap(Expression::Variable(self))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocExpr {
    pub loc: Loc,
    pub expr: Expression,
}

impl LocExpr {
    pub fn new(loc: Loc, expr: Expression) -> LocExpr {
        LocExpr {
            loc,
            expr,
        }
    }

    pub fn boxed(self) -> Box<LocExpr> {
        Box::new(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    String(String),
    Int(i64),
    FuncCall(FuncCall),
    Touple(Vec<LocExpr>),
    VarDecl(Box<VarDecl>),
    ReAssignment(Box<ReAssignment>),
    Variable(Variable),
    FuncExpr(FuncExpr),
    Null,
    Return(Box<LocExpr>),
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
    BuiltinFunc(BuiltinFunc),
}
impl Expression {
    pub fn with_loc(&self, loc: &Loc) -> LocExpr {
        loc.wrap(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeLiteral {
    pub start: LocExpr,
    pub end: LocExpr
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuiltinFunc {
    pub loc: Loc,
    pub name: BuiltinName,
    pub this: Box<LocExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LazyExpression {
    pub expr: LocExpr,
    pub scope: Rc<Scope>,
}

impl LazyExpression {
    pub fn eval<W: Write>(
        &self,
        ctx: &FunCtx<W>
    ) -> Result<LocExpr, LocError> {
        eval_expr_and_call_returned_block(&self.expr, ctx)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportExpr {
    pub loc: Loc,
    pub path: String,
    pub symbols: Vec<Variable>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Access {
    pub loc: Loc,
    pub target: Box<LocExpr>,
    pub key: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub items: Rc<RefCell<Vec<LocExpr>>>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: LocExpr,
    pub then_expr: LocExpr,
    pub else_expr: Option<LocExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForExpr {
    pub identifier: Variable,
    pub range: Box<RangeLiteral>,
    pub body: Vec<LocExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    pub condition: Box<LocExpr>,
    pub body: Vec<LocExpr>,
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
    pub expr: LocExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReAssignment {
    pub target: VarOrAccess,
    pub expr: LocExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarOrAccess {
    Access(Access),
    Variable(Variable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub accessors: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncExpr {
    pub args: Vec<String>,
    pub expressions: Vec<LocExpr>,
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

    pub fn wrap(&self, expr: Expression) -> LocExpr {
        LocExpr {
            loc: self.clone(),
            expr,
        }
    }

    pub fn touple(&self, touple: Vec<LocExpr>) -> LocExpr {
        self.wrap(Expression::Touple(touple))
    }

    pub fn string(&self, string: String) -> LocExpr {
        self.wrap(Expression::String(string))
    }

    pub fn str(&self, string: &str) -> LocExpr {
        self.wrap(Expression::String(string.to_string()))
    }

    pub fn null(&self) -> LocExpr {
        self.wrap(Expression::Null)
    }

    pub fn func_call(&self, func_call: FuncCall) -> LocExpr {
        self.wrap(Expression::FuncCall(func_call))
    }

    pub fn var_decl(&self, var_decl: VarDecl) -> LocExpr {
        self.wrap(Expression::VarDecl(Box::new(var_decl)))
    }

    pub fn int(&self, int: i64) -> LocExpr {
        self.wrap(Expression::Int(int))
    }

    pub fn builtin_func(&self, builtin_func: BuiltinFunc) -> LocExpr {
        self.wrap(Expression::BuiltinFunc(builtin_func))
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

    pub fn var(&self) -> Variable {
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

