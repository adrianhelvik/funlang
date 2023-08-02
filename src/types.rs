use colorful::Color;
use colorful::Colorful;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

impl Program {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Program { expressions }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub ident: Variable,
    pub arg: Box<Expression>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Expression,
    pub then_expr: Expression,
    pub else_expr: Option<Expression>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReAssignment {
    pub ident: String,
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncExpr {
    pub args: Vec<String>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug)]
pub struct Scope {
    pub values: Box<RefCell<HashMap<String, Expression>>>,
    pub parent: RefCell<Option<Rc<Scope>>>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            values: Box::new(RefCell::new(HashMap::new())),
            parent: RefCell::new(None),
        }
    }

    pub fn create(parent: Rc<Scope>) -> Self {
        Scope {
            values: Box::new(RefCell::new(HashMap::new())),
            parent: RefCell::new(Some(parent)),
        }
    }

    pub fn assign(&self, ident: String, expr: Expression) -> Expression {
        self.values
            .borrow_mut()
            .insert(ident.to_string(), expr.clone());
        expr
    }

    pub fn set(&self, ident: String, expr: Expression) -> Expression {
        let val = {
            let values = self.values.borrow();
            values.get(&ident).cloned()
        };
        match val {
            Some(_) => {
                self.values
                    .borrow_mut()
                    .insert(ident.to_string(), expr.clone());
                expr
            }
            None => match self.parent.take() {
                Some(parent) => parent.set(ident, expr),
                None => {
                    panic!("Attempted to assign to undefined variable {}", ident);
                }
            },
        }
    }

    pub fn has(&self, ident: String) -> bool {
        self.values.borrow().contains_key(&ident)
    }

    pub fn get(&self, ident: String) -> Option<Expression> {
        match self.values.borrow().get(&ident) {
            Some(expr) => Some(expr.clone()),
            None => {
                let parent = self.parent.borrow();
                match parent.as_ref() {
                    Some(parent) => parent.get(ident),
                    None => None,
                }
            }
        }
    }
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
}

pub type Tokens = Vec<Token>;

#[derive(Debug, PartialEq, Clone)]
pub struct LocError {
    pub message: String,
    pub loc: Loc,
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
}

#[derive(Debug, PartialEq)]
pub enum Possibly<T, E> {
    Some(T),
    Err(E),
    None,
}

impl<T, E: std::fmt::Debug> Possibly<T, E> {
    pub fn unwrap(self) -> T {
        match self {
            Possibly::Some(expr) => expr,
            Possibly::Err(err) => panic!("Syntax error: {:#?}", err),
            Possibly::None => panic!("Attempted to unwrap None"),
        }
    }
}
