use colorful::Color;
use colorful::Colorful;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::rc::Rc;

use crate::builtins::*;
use crate::interpreter::call_func_expr;
use crate::interpreter::eval_expr_and_call_returned_block;
use crate::interpreter::eval_expr_list;
use crate::lexer::lex;
use crate::parser::parse;

fn expr_list_debug_str(delimiter: &str, expressions: &Vec<Expression>) -> String {
    join(
        delimiter,
        expressions.iter().map(|expr| expr.debug_str()).collect(),
    )
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expressions: Vec<Expression>,
    pub filename: Option<String>
}

impl Program {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Program { expressions, filename: None }
    }

    pub fn debug_str(&self) -> String {
        return expr_list_debug_str("\n", &self.expressions);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub ident: Identifier,
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

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub items: Vec<Expression>,
    pub loc: Loc,
}

impl List {
    pub fn eval<W: Write>(
        &self,
        output: &Rc<RefCell<W>>,
        scope: &Rc<Scope>,
    ) -> Result<List, LocError> {
        let mut items = vec![];

        for expr in &self.items {
            items.push(expr.eval(output, scope)?);
        }

        Ok(List {
            loc: self.loc.clone(),
            items,
        })
    }

    fn as_string<W: Write>(
        &self,
        output: &Rc<RefCell<W>>,
        scope: &Rc<Scope>,
    ) -> Result<String, LocError> {
        let mut stringified_items = vec![];

        for item in &self.items {
            stringified_items.push(item.as_string(&self.loc, output, scope)?);
        }

        return Ok(format!("[{}]", join(", ", stringified_items)));
    }
}

fn expressions_to_debug_str(expressions: &Vec<Expression>) -> String {
    join(", ", expressions.iter().map(|e| e.debug_str()).collect())
}

fn join(sep: &str, values: Vec<String>) -> String {
    if let Some(s) = values.get(0) {
        let mut result = s.to_string();
        for s in values.iter().skip(1) {
            result.push_str(sep);
            result.push_str(&s);
        }
        result
    } else {
        String::new()
    }
}

impl Expression {
    pub fn debug_str(&self) -> String {
        match self {
            Expression::String(s) => format!("\"{}\"", s.clone()),
            Expression::Int(i) => i.to_string(),
            Expression::FuncCall(_f) => "func_call".to_string(),
            Expression::List(list) => format!("[{}]", expr_list_debug_str(", ", &list.items)),
            Expression::Access(access) => format!("{}.{}", access.target.debug_str(), access.key),
            Expression::ImportExpr(import_expr) => {
                let imported = import_expr.symbols.iter().map(|t| t.value.clone()).collect();
                return format!("from \"{}\" import {}", import_expr.source, join(", ", imported));
            },
            Expression::ForExpr(for_expr) => {
                let body_debug_str = join(
                    "\n",
                    for_expr.body.iter().map(|expr| expr.debug_str()).collect(),
                );
                format!(
                    "for {} in {}..{} {{ {} }}",
                    for_expr.identifier.ident,
                    for_expr.start.debug_str(),
                    for_expr.end.debug_str(),
                    body_debug_str
                )
            }
            Expression::WhileExpr(while_expr) => {
                let body_debug_str = join(
                    "\n",
                    while_expr
                        .body
                        .iter()
                        .map(|expr| expr.debug_str())
                        .collect(),
                );
                format!(
                    "while {} {{ {} }}",
                    while_expr.condition.debug_str(),
                    body_debug_str
                )
            }
            Expression::Touple(expressions) => {
                format!("({})", expressions_to_debug_str(expressions))
            }
            Expression::Return(inner) => format!("return {}", inner.debug_str()),
            Expression::Variable(variable) => format!("var({})", variable.ident),
            Expression::VarDecl(var_decl) => {
                format!("let {} = {}", var_decl.ident, var_decl.expr.debug_str())
            }
            Expression::ReAssignment(re_assignment) => format!(
                "{:#?} = {}",
                re_assignment.ident,
                re_assignment.expr.debug_str()
            ),
            Expression::FuncExpr(func_expr) => {
                let args_str = join(", ", func_expr.args.iter().map(|it| it.clone()).collect());
                let body_str = func_expr
                    .expressions
                    .iter()
                    .map(|it| it.debug_str())
                    .fold(String::new(), |a, b| format!("{}\n{}", a, b));
                format!("({}) {}", args_str, body_str)
            }
            Expression::Null => String::from("null"),
            Expression::Closure(closure) => {
                return format!(
                    "{{\n{}\n}}",
                    closure
                        .func_expr
                        .expressions
                        .iter()
                        .map(|it| it.debug_str())
                        .fold(String::new(), |a, b| format!("{}\n{}", a, b))
                );
            }
            Expression::IfExpr(if_expr) => {
                if let Some(else_expr) = &if_expr.else_expr {
                    return format!(
                        "if {} {} else {}",
                        if_expr.condition.debug_str(),
                        if_expr.then_expr.debug_str(),
                        else_expr.debug_str()
                    );
                } else {
                    return format!(
                        "if {} {}",
                        if_expr.condition.debug_str(),
                        if_expr.then_expr.debug_str()
                    );
                }
            }
            Expression::Bool(value) => value.to_string(),
            Expression::Block(func_call) => {
                format!("{} {}", func_call.ident.debug_str(), func_call.arg.debug_str())
            }
            Expression::Map(map) => {
                let map_str = map
                    .borrow()
                    .iter()
                    .map(|(k, v)| {
                        return format!("{} = {}", k, v.debug_str());
                    })
                    .fold(String::new(), |a, b| format!("{}, {}", a, b));
                format!("Map {{ {} }}", map_str)
            }
        }
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            Expression::String(_) => "string",
            Expression::Int(_) => "integer",
            Expression::FuncCall(_) => "function_call",
            Expression::Touple(_) => "touple",
            Expression::VarDecl(_) => "variable_declaration",
            Expression::ReAssignment(_) => "re_assignment",
            Expression::Variable(_) => "variable",
            Expression::FuncExpr(_) => "function",
            Expression::Null => "null",
            Expression::Return(_) => "return_expression",
            Expression::Closure(_) => "closure",
            Expression::IfExpr(_) => "if",
            Expression::Bool(_) => "boolean",
            Expression::Block(_) => "boolean",
            Expression::Map(_) => "map",
            Expression::ForExpr(_) => "for",
            Expression::WhileExpr(_) => "while",
            Expression::List(_) => "list",
            Expression::Access(_) => "access",
            Expression::ImportExpr(_) => "import",
        }
    }

    pub fn as_vec(&self) -> Vec<Expression> {
        match self {
            Expression::Touple(expressions) => expressions.clone(),
            other => vec![other.clone()],
        }
    }

    pub fn as_int<W: Write>(
        &self,
        loc: &Loc,
        output: &Rc<RefCell<W>>,
        scope: &Rc<Scope>,
    ) -> Result<i64, LocError> {
        match self.eager_eval(output, scope)? {
            Expression::Int(int) => Ok(int.clone()),
            _ => Err(loc.error(&format!("Failed to convert {} to Int", self.type_str()))),
        }
    }

    pub fn unnest(&self) -> &Expression {
        let mut expr = self;

        loop {
            match expr {
                Expression::Touple(inner) => {
                    if inner.len() != 1 {
                        return expr;
                    } else {
                        expr = &inner[0]
                    }
                }
                expr => return expr,
            }
        }
    }

    pub fn eager_eval<W: Write>(
        &self,
        output: &Rc<RefCell<W>>,
        scope: &Rc<Scope>,
    ) -> Result<Expression, LocError> {
        match self.eval(output, scope)? {
            Expression::Return(expr) => Ok(*expr),
            other => Ok(other),
        }
    }

    pub fn strip_return(&self) -> Expression {
        match self {
            Expression::Return(expr) => *expr.clone(),
            other => other.clone(),
        }
    }

    pub fn get_by_key(&self, key: &str) -> Option<Expression> {
        match self {
            Expression::Map(map) => {
                map.borrow().get(key).cloned()
            },
            _ => None,
        }
    }

    pub fn set(&self, key: &str, val: Expression) -> bool {
        match self {
            Expression::Map(map) => {
                map.borrow_mut().insert(key.to_string(), val);
                true
            },
            _ => false,
        }
    }

    pub fn eval<W: Write>(
        &self,
        output: &Rc<RefCell<W>>,
        scope: &Rc<Scope>,
    ) -> Result<Expression, LocError> {
        match self {
            Expression::FuncCall(func_call) => {
                if func_call.ident.accessors.len() == 1 {
                    let name = func_call.ident.accessors[0].value.clone();
                    match name.as_ref() {
                        "print" => return fun_print(&func_call, output, scope),
                        "println" => return fun_println(&func_call, output, scope),
                        "add" => return fun_add(scope, output, &func_call),
                        "sub" => return fun_sub(scope, output, &func_call),
                        "eq" => return fun_eq(scope, output, &*func_call.arg),
                        "str" => return fun_str(scope, output, &*func_call),
                        "not" => return fun_not(scope, output, &*func_call),
                        "lte" => return fun_lte(scope, output, &*func_call),
                        "lt" => return fun_lt(scope, output, &*func_call),
                        "gte" => return fun_gte(scope, output, &*func_call),
                        "or" => return fun_or(&scope, output, &*func_call),
                        "type" => return fun_type(&scope, output, &func_call),
                        "etype" => return fun_etype(&*func_call.arg),
                        "and" => return fun_and(&scope, output, &*func_call),
                        "modulo" => return fun_modulo(scope, output, &*func_call),
                        "Map" => return fun_create_map(scope, output, &*func_call.arg),
                        _ => {}
                    }
                }

                let expr = call_func_expr(func_call, output, scope);
                Ok(expr?.strip_return())
            },
            Expression::Block(func_call) => call_func_expr(func_call, output, scope),
            Expression::VarDecl(assignment) => {
                let value = assignment.expr.eval(output, scope)?;
                scope.assign(assignment.ident.clone(), value.clone());
                Ok(value)
            }
            Expression::ReAssignment(reassignment) => {
                let assigned_value = reassignment.expr.eval(output, scope)?;

                if reassignment.ident.accessors.len() == 1 {
                    let token = reassignment.ident.accessors[0].clone();
                    scope.set(&token.loc, token.value, assigned_value.clone())?;
                    return Ok(assigned_value.clone());
                }

                let mut container = scope.get(&reassignment.ident.accessors[0].value)
                    .ok_or_else(|| reassignment.ident.accessors[0].loc.error("Undefined access"))?;

                for i in 1..(reassignment.ident.accessors.len() - 1) {
                    let token = &reassignment.ident.accessors[i];
                    container = container.get_by_key(&token.value)
                        .ok_or_else(|| token.loc.error(&format!("Failed to read property '{}'", &token.value)))?;
                }
                let token = reassignment.ident.accessors.last().unwrap();
                if !container.set(&token.value, assigned_value.clone()) {
                    return Err(reassignment.ident.accessors[0].loc.error(&format!("Failed to set property '{}'", &token.value)));
                }
                Ok(assigned_value)
            }
            Expression::String(string) => Ok(Expression::String(string.clone())),
            Expression::Touple(value) => {
                if value.len() == 1 {
                    return Ok(value[0].clone().eval(output, scope)?);
                }
                Ok(Expression::Touple(value.clone()))
            }
            Expression::Int(value) => Ok(Expression::Int(*value)),
            Expression::Variable(var) => {
                if var.ident == "true" {
                    return Ok(Expression::Bool(true));
                }
                if var.ident == "false" {
                    return Ok(Expression::Bool(false));
                }
                if var.ident == "null" {
                    return Ok(Expression::Null);
                }
                if let Some(value) = scope.get(&var.ident) {
                    return Ok(value);
                }
                var.not_defined_err()
            }
            Expression::FuncExpr(func_expr) => Ok(Expression::Closure(Box::new(Closure {
                func_expr: func_expr.clone(),
                scope: Rc::clone(&scope),
            }))),
            Expression::ForExpr(for_expr) => {
                let ident = &for_expr.identifier;
                let start = for_expr.start.as_int(&ident.loc, output, scope)?;
                let end = for_expr.end.as_int(&ident.loc, output, scope)?;
                let body = &for_expr.body;

                let mut last = Expression::Null;

                for i in start..end {
                    let scope = Rc::new(Scope::create(scope));
                    scope.assign(ident.ident.clone(), Expression::Int(i));
                    last = eval_expr_list(&body, output, &scope)?;
                }

                return Ok(last);
            }
            Expression::WhileExpr(for_expr) => {
                let condition = &for_expr.condition;
                let body = &for_expr.body;

                let mut last = Expression::Null;

                while condition.eval(output, scope)?.as_bool()? {
                    let scope = Rc::new(Scope::create(scope));
                    last = eval_expr_list(&body, output, &scope)?;
                }

                return Ok(last);
            }
            Expression::IfExpr(if_expr) => {
                let condition = if_expr.condition.eager_eval(output, scope)?.as_bool()?;
                if condition {
                    eval_expr_and_call_returned_block(&if_expr.then_expr, output, scope)
                } else if let Some(else_expr) = &if_expr.else_expr {
                    eval_expr_and_call_returned_block(&else_expr, output, scope)
                } else {
                    Ok(Expression::Null)
                }
            }
            Expression::Return(inner) => {
                let inner = inner.eval(output, scope)?;
                return Ok(Expression::Return(Box::new(inner)));
            }
            Expression::Bool(value) => Ok(Expression::Bool(value.clone())),
            Expression::Null => Ok(Expression::Null),
            Expression::List(list) => Ok(Expression::List(list.eval(output, scope)?)),
            Expression::Access(access) => match access.target.eval(output, scope)? {
                Expression::Map(map) => {
                    let result = map
                        .borrow()
                        .get(&access.key)
                        .ok_or_else(|| access.loc.error("Not in map"))?
                        .clone();
                    Ok(result)
                }
                Expression::List(_) => {
                    todo!()
                }
                _ => Err(access.loc.error("Not able to call method on value")),
            },
            Expression::ImportExpr(import_expr) => {
                let bytes = fs::read(&import_expr.source)
                    .map_err(|_| import_expr.loc.error(&format!("Failed to import '{}'", import_expr.source)))?;

                let source = String::from_utf8(bytes)
                    .map_err(|_| import_expr.loc.error(&format!("Failed to import '{}'", import_expr.source)))?;

                let tokens = &lex(&source);

                let ast = parse(tokens)?;

                let remote_scope = Rc::new(Scope::new());

                eval_expr_list(&ast.expressions, output, &remote_scope)?;

                for ident in &import_expr.symbols {
                    println!("Importing {}", ident.value);
                    let value = remote_scope.get(&ident.value)
                        .ok_or_else(|| import_expr.loc.error(&format!("Symbol '{}' was not exported from '{}'", &ident.value, import_expr.source)))?;

                    scope.assign(ident.value.clone(), value);
                }

                Ok(Expression::Null)
            },
            _ => {
                todo!("Failed to eval expression {:?}", self);
            }
        }
    }

    pub fn as_string<W: Write>(
        &self,
        loc: &Loc,
        output: &Rc<RefCell<W>>,
        scope: &Rc<Scope>,
    ) -> Result<String, LocError> {
        match self {
            Expression::String(string) => Ok(string.clone()),
            Expression::Int(int) => Ok(int.to_string()),
            Expression::Touple(value) => {
                let mut result = String::new();
                for expr in value {
                    let item = expr.eval(output, scope);
                    match item {
                        Ok(item) => match item.as_string(loc, output, scope) {
                            Ok(res) => {
                                result.push_str(&res);
                            }
                            Err(e) => return Err(e),
                        },
                        Err(e) => return Err(e),
                    }
                }
                Ok(result)
            }
            Expression::Bool(value) => Ok(value.to_string()),
            Expression::Null => Ok(String::from("null")),
            Expression::Return(expression) => expression.as_string(loc, output, scope),
            Expression::List(list) => list.as_string(output, scope),
            // TODO: Split Expression and SimpleExpression
            Expression::FuncCall(_func_call) => todo!(),
            Expression::Access(_) => todo!(),
            Expression::VarDecl(_var_decl) => todo!(),
            Expression::ReAssignment(_re_assignment) => todo!(),
            Expression::Variable(var) => scope
                .get(&var.ident)
                .ok_or(var.loc.error("Could not resolve variable"))?
                .as_string(&var.loc, output, scope),
            Expression::FuncExpr(_func_expr) => todo!(),
            Expression::Closure(_closure) => todo!(),
            Expression::IfExpr(_if_expr) => todo!(),
            Expression::Block(_func_call) => todo!(),
            Expression::Map(_map) => todo!(),
            Expression::ForExpr(_) => todo!(),
            Expression::WhileExpr(_) => todo!(),
            Expression::ImportExpr(_) => todo!(),
        }
    }

    pub fn as_bool(&self) -> Result<bool, LocError> {
        match self.unnest() {
            Expression::Bool(value) => Ok(value.clone()),
            Expression::Null => Ok(true),
            _ => Ok(true),
        }
    }

    pub(crate) fn require_two_touple(&self) -> Option<(Expression, Expression)> {
        let expressions = self.as_vec();

        if expressions.len() == 2 {
            Some((expressions[0].clone(), expressions[1].clone()))
        } else {
            None
        }
    }
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
    pub start: Box<Expression>,
    pub end: Box<Expression>,
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

impl Identifier {
    pub fn debug_str(&self) -> String {
        join(".", self.accessors.iter().map(|t| t.value.clone()).collect())
    }

    pub fn error(&self, message: &str) -> LocError {
        self.accessors[0].loc.error(message)
    }

    pub fn loc(&self) -> Loc {
        self.accessors[0].loc.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncExpr {
    pub args: Vec<String>,
    pub expressions: Vec<Expression>,
}

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

    pub fn set(&self, loc: &Loc, ident: String, expr: Expression) -> Result<Expression, LocError> {
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
                parent.set(&loc, ident, expr)
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

    pub fn debug(&self) {
        println!("<scope debug>");
        self.do_debug();
        println!("</scope debug>");
    }

    fn do_debug(&self) {
        println!("{:?}", self.values.borrow().keys());
        if self.parent != None {
            self.parent.clone().unwrap().borrow().do_debug();
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
