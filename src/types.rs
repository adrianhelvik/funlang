#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub ident: String,
    pub arg: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    String(String),
    Int(i64),
    FuncCall(FuncCall),
    Touple(Vec<Expression>),
    Assignment(Box<Assignment>),
    Variable(String),
    Block(Block),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub ident: String,
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub expressions: Vec<Expression>,
}
