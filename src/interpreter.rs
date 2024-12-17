use crate::builtins::*;
use crate::context::FunCtx;
use crate::lexer::lex;
use crate::parser::parse;
use crate::scope::Scope;
use crate::types::*;
use crate::util::join;
use resolve_path::PathResolveExt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

pub fn eval_program<W: Write>(
    program: &Program,
    output: &Rc<RefCell<W>>,
    filename: &str,
) -> Result<(), LocError> {
    let scope = Rc::new(Scope::new());

    scope.assign(
        "__filename".to_string(),
        Expression::String(filename.to_string()),
    );

    let ctx = FunCtx {
        scope,
        output: Rc::clone(output),
        filename: filename.to_string(),
    };

    eval_expr_list(&program.expressions, &ctx)?;

    Ok(())
}

pub fn eval_expr_list<W: Write>(
    expressions: &Vec<Expression>,
    ctx: &FunCtx<W>,
) -> Result<Expression, LocError> {
    let mut value = Expression::Null;

    for expr in expressions {
        value = expr.eval(ctx)?;

        if let Expression::Return(result) = value {
            return Ok(Expression::Return(result));
        }
    }

    Ok(value)
}

pub fn eval_expr_and_call_returned_block<W: Write>(
    expr: &Expression,
    ctx: &FunCtx<W>,
) -> Result<Expression, LocError> {
    let res = expr.eval(ctx)?;

    match res {
        Expression::Closure(closure) => {
            let child_scope = Scope::create(&ctx.scope);
            child_scope.assign(
                String::from("__block__"),
                Expression::FuncExpr(closure.func_expr),
            );
            let child_ctx = FunCtx {
                scope: Rc::new(child_scope),
                output: Rc::clone(&ctx.output),
                filename: ctx.filename.clone(),
            };
            // TODO: Consider if I want this
            let value = Expression::Block(FuncCall {
                ident: Identifier {
                    accessors: vec![Token {
                        value: "__block__".to_string(),
                        loc: Loc::new(0, 0),
                    }],
                },
                arg: Box::new(Expression::Null),
                this: Box::new(Expression::Null),
            })
            .eval(&child_ctx)?;
            Ok(value)
        }
        other => Ok(other),
    }
}

pub fn lookup(scope: &Rc<Scope>, ident: &Identifier) -> Result<(Expression, Loc), LocError> {
    let mut this = Expression::Null;

    let mut value = scope
        .get(&ident.accessors[0].value)
        .ok_or_else(|| ident.accessors[0].loc.error("Failed to look up variable"))?;

    for i in 1..ident.accessors.len() {
        let token = &ident.accessors[i];
        this = value.clone();
        value = value.get_by_key_token(&token)?;
    }

    let value = match value {
        Expression::FuncCall(func_call) => {
            Expression::FuncCall(func_call.with_this(this))
        }
        other => other
    };

    Ok((value, ident.accessors.last().unwrap().loc.clone()))
}

pub fn call_func_expr<W: Write>(
    func_call: &FuncCall,
    ctx: &FunCtx<W>,
) -> Result<Expression, LocError> {
    let (expression, loc) = lookup(&ctx.scope, &func_call.ident)?;

    match expression {
        Expression::Closure(closure) => call_func(
            &closure.func_expr,
            func_call,
            ctx,
            &FunCtx {
                scope: Rc::clone(&closure.scope),
                output: Rc::clone(&ctx.output),
                filename: ctx.filename.clone(),
            },
        ),
        Expression::FuncExpr(func_expr) => call_func(&func_expr, &func_call, ctx, ctx),
        Expression::Map(map) => call_map(&loc, &map, func_call, ctx),
        Expression::BuiltinFuncCall(builtin) => {
            match builtin.name {
                BuiltinName::ListPush => fun_list_push(ctx, &func_call.with_this(*builtin.this)),
                BuiltinName::ListLen => fun_list_len(ctx, &func_call.with_this(*builtin.this)),
            }
        },
        expression => Err(loc.error(&format!(
            "expression of type '{}' is not callable. (value = {})",
            expression.type_str(),
            expression.debug_str()
        ))),
    }
}

fn call_map<W: Write>(
    loc: &Loc,
    map: &Rc<RefCell<HashMap<String, Expression>>>,
    func_call: &FuncCall,
    ctx: &FunCtx<W>,
) -> Result<Expression, LocError> {
    let args = func_call.arg.as_vec();

    match args.len() {
        1 => {
            let key = args[0].eval(ctx)?.as_string(loc, ctx)?;
            let val = match map.borrow().get(&key) {
                Some(expr) => expr.clone(),
                None => Expression::Null,
            };
            Ok(val)
        }
        2 => {
            let key = args[0].eval(ctx)?.as_string(loc, ctx)?;
            let val = args[1].eval(ctx)?;
            map.borrow_mut().insert(key, val.clone());
            Ok(val)
        }
        _ => {
            panic!("Called map with a number of arguments not in [1, 2]");
        }
    }
}

pub fn call_func<W: Write>(
    func_expr: &FuncExpr,
    func_call: &FuncCall,
    ctx: &FunCtx<W>,
    enclosing_ctx: &FunCtx<W>,
) -> Result<Expression, LocError> {
    let function_ctx = FunCtx {
        // Scoped to the enclosing context
        scope: Rc::new(Scope::create(&enclosing_ctx.scope)),
        output: Rc::clone(&ctx.output),
        filename: ctx.filename.clone(),
    };

    let passed_in_args = func_call.arg.as_vec();

    for (i, ident) in func_expr.args.iter().enumerate() {
        if let Some(arg) = passed_in_args.get(i) {
            let value = arg.eval(ctx)?;
            function_ctx.scope.assign(ident.clone(), value);
        } else {
            function_ctx.scope.assign(ident.clone(), Expression::Null);
        }
    }

    let result = eval_expr_list(&func_expr.expressions, &function_ctx)?;

    Ok(result)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{lexer::lex, parser::parse};

    use super::*;

    fn eval_ast(ast: &Program) -> String {
        let output = Rc::new(RefCell::new(Vec::new()));

        if let Err(err) = eval_program(ast, &output, "./test.fun") {
            panic!("Failed to eval ast {:#?}", err);
        }

        let output = output.borrow().clone();

        return String::from_utf8(output).unwrap();
    }

    fn eval(code: &str) -> String {
        let program = parse(&lex(code)).unwrap();
        eval_ast(&program)
    }

    #[test]
    pub fn you_can_set_a_variable() {
        let output = eval(
            "
            let x = 123
            println x
        ",
        );
        assert_eq!(output, "123\n");
    }

    #[test]
    pub fn you_can_update_a_variable() {
        let output = eval(
            r#"
            let x = 1
            x = 2
            println x
        "#,
        );

        assert_eq!(output, "2\n");
    }
}

impl ImportExpr {
    pub fn is_relative(&self) -> Result<bool, LocError> {
        let chars = self.source.chars().collect::<Vec<char>>();

        if let Some(ch) = chars.get(0) {
            return Ok(*ch == '.' || *ch == '/');
        }

        Err(self.loc.error("Invalid import specifier"))
    }

    fn resolve_relative<W: Write>(&self, ctx: &FunCtx<W>) -> Result<String, LocError> {
        let current_filename = ctx.filename.clone();

        let current_dir = Path::new(&current_filename).parent().ok_or_else(|| {
            self.loc
                .error(&format!("Failed to import '{}'", self.source))
        })?;

        let resolved = self.source.resolve_in(current_dir);

        let final_path = resolved.to_str().ok_or_else(|| {
            self.loc
                .error(&format!("Failed to import '{}'", self.source))
        })?;

        Ok(final_path.to_string())
    }

    fn could_not_resolve<T>(&self) -> Result<T, LocError> {
        Err(self
            .loc
            .error(&format!("Could not resolve '{}'", self.source)))
    }

    pub fn eval<W: Write>(&self, ctx: &FunCtx<W>) -> Result<Expression, LocError> {
        let remote_scope = if self.is_relative()? {
            self.get_scope_of_relative_import(ctx)?
        } else {
            self.get_scope_of_external_import()?
        };

        for ident in &self.symbols {
            let value = remote_scope.get(&ident.value).ok_or_else(|| {
                ident.loc.error(&format!(
                    "Symbol '{}' was not exported from '{}'",
                    &ident.value, self.source
                ))
            })?;

            ctx.scope.assign(ident.value.clone(), value);
        }

        Ok(Expression::Null)
    }

    pub fn get_scope_of_external_import(&self) -> Result<Rc<Scope>, LocError> {
        if self.source == "core" {
            Ok(Rc::new(fun_core_module()))
        } else {
            self.could_not_resolve()
        }
    }

    pub fn get_scope_of_relative_import<W: Write>(
        &self,
        ctx: &FunCtx<W>,
    ) -> Result<Rc<Scope>, LocError> {
        let filename = &self.resolve_relative(ctx)?;

        let bytes = fs::read(filename).map_err(|_| {
            self.loc
                .error(&format!("Failed to import '{}'", self.source))
        })?;

        let source = String::from_utf8(bytes).map_err(|_| {
            self.loc
                .error(&format!("Failed to import '{}'", self.source))
        })?;

        let tokens = &lex(&source);
        let ast = parse(tokens)?;

        let remote_scope = Rc::new(Scope::new());

        remote_scope.assign(
            "__filename".to_string(),
            Expression::String(filename.clone()),
        );

        let remote_ctx = FunCtx {
            scope: Rc::clone(&remote_scope),
            output: Rc::clone(&ctx.output),
            filename: filename.clone(),
        };
        eval_expr_list(&ast.expressions, &remote_ctx)?;

        Ok(remote_scope)
    }
}

impl Program {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Program { expressions }
    }

    pub fn debug_str(&self) -> String {
        return expr_list_debug_str("\n", &self.expressions);
    }
}

impl Identifier {
    pub fn debug_str(&self) -> String {
        join(
            ".",
            self.accessors.iter().map(|t| t.value.clone()).collect(),
        )
    }

    pub fn error(&self, message: &str) -> LocError {
        self.accessors[0].loc.error(message)
    }

    pub fn loc(&self) -> Loc {
        self.accessors[0].loc.clone()
    }
}

fn expr_list_debug_str(delimiter: &str, expressions: &Vec<Expression>) -> String {
    join(
        delimiter,
        expressions.iter().map(|expr| expr.debug_str()).collect(),
    )
}

impl Expression {
    pub fn is_touple(&self) -> bool {
        match self {
            Expression::Touple(_) => true,
            _ => false,
        }
    }

    pub fn debug_str(&self) -> String {
        match self {
            Expression::String(s) => format!("\"{}\"", s.clone()),
            Expression::Int(i) => i.to_string(),
            Expression::FuncCall(_f) => "func_call".to_string(),
            Expression::List(list) => format!("[{}]", expr_list_debug_str(", ", &list.items.borrow())),
            Expression::Access(access) => format!("{}.{}", access.target.debug_str(), access.key),
            Expression::Lazy(lazy_expr) => format!("lazy {}", lazy_expr.expr.debug_str()),
            Expression::BuiltinFuncCall(builtin) => builtin.as_string(),
            Expression::ImportExpr(import_expr) => {
                let imported = import_expr
                    .symbols
                    .iter()
                    .map(|t| t.value.clone())
                    .collect();
                return format!(
                    "from \"{}\" import {}",
                    import_expr.source,
                    join(", ", imported)
                );
            }
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
                format!(
                    "{} {}",
                    func_call.ident.debug_str(),
                    func_call.arg.debug_str()
                )
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
            Expression::Lazy(_) => "lazy",
            Expression::BuiltinFuncCall(_) => "builtin",
        }
    }

    pub fn as_vec(&self) -> Vec<Expression> {
        match self {
            Expression::Touple(expressions) => expressions.clone(),
            other => vec![other.clone()],
        }
    }

    pub fn as_int<W: Write>(&self, loc: &Loc, ctx: &FunCtx<W>) -> Result<i64, LocError> {
        match self.eager_eval(ctx)? {
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

    pub fn eager_eval<W: Write>(&self, ctx: &FunCtx<W>) -> Result<Expression, LocError> {
        match self.eval(ctx)? {
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

    pub fn get_by_key_and_loc(&self, key: &str, loc: &Loc) -> Result<Expression, LocError> {
        match self {
            Expression::Map(map) => map.borrow().get(key).cloned().ok_or_else(|| {
                loc.error(&format!(
                    "Map does not contain key \"{}\". Available keys: [{}]",
                    key,
                    join(
                        ", ",
                        map.borrow()
                            .keys()
                            .map(|key| format!("\"{}\"", key))
                            .collect()
                    )
                ))
            }),
            Expression::List(list) => list
                .get(key, loc)
                .ok_or_else(|| loc.error(&format!("Could not find a method named \"{}\"", key))),
            _ => Err(loc.error(&format!(
                "could not look up \"{}\" in '{}'",
                key,
                self.type_str()
            ))),
        }
    }

    pub fn get_by_key_token(&self, token: &Token) -> Result<Expression, LocError> {
        self.get_by_key_and_loc(&token.value, &token.loc)
    }

    pub fn set(&self, key: &str, val: Expression) -> bool {
        match self {
            Expression::Map(map) => {
                map.borrow_mut().insert(key.to_string(), val);
                true
            }
            _ => false,
        }
    }

    pub fn eval<W: Write>(&self, ctx: &FunCtx<W>) -> Result<Expression, LocError> {
        match self {
            Expression::FuncCall(func_call) => {
                if func_call.ident.accessors.len() == 1 {
                    let name = func_call.ident.accessors[0].value.clone();
                    match name.as_ref() {
                        "print" => return fun_print(ctx, &func_call),
                        "println" => return fun_println(ctx, &func_call),
                        "add" => return fun_add(ctx, &func_call),
                        "sub" => return fun_sub(ctx, &func_call),
                        "eq" => return fun_eq(ctx, &func_call),
                        "str" => return fun_str(ctx, &*func_call),
                        "not" => return fun_not(ctx, &*func_call),
                        "lte" => return fun_lte(ctx, &*func_call),
                        "lt" => return fun_lt(ctx, &*func_call),
                        "gte" => return fun_gte(ctx, &*func_call),
                        "or" => return fun_or(ctx, &*func_call),
                        "type" => return fun_type(ctx, &func_call),
                        "etype" => return fun_etype(&func_call),
                        "and" => return fun_and(ctx, &*func_call),
                        "modulo" => return fun_modulo(ctx, &*func_call),
                        "Map" => return fun_create_map(),
                        "lazy" => return fun_lazy(ctx, &func_call),
                        "in" => return fun_in(ctx, func_call),
                        _ => {}
                    }
                }

                let expr = call_func_expr(func_call, ctx);
                Ok(expr?.strip_return())
            }
            Expression::Block(func_call) => call_func_expr(func_call, ctx),
            Expression::VarDecl(assignment) => {
                let value = assignment.expr.eval(ctx)?;
                ctx.scope.assign(assignment.ident.clone(), value.clone());
                Ok(value)
            }
            Expression::ReAssignment(reassignment) => {
                let assigned_value = reassignment.expr.eval(ctx)?;

                if reassignment.ident.accessors.len() == 1 {
                    let token = reassignment.ident.accessors[0].clone();
                    ctx.scope
                        .reassign(&token.loc, token.value, assigned_value.clone())?;
                    return Ok(assigned_value.clone());
                }

                let mut container = ctx
                    .scope
                    .get(&reassignment.ident.accessors[0].value)
                    .ok_or_else(|| {
                        reassignment.ident.accessors[0]
                            .loc
                            .error("Undefined access")
                    })?;

                for i in 1..(reassignment.ident.accessors.len() - 1) {
                    let token = &reassignment.ident.accessors[i];
                    container = container.get_by_key_token(&token)?;
                }
                let token = reassignment.ident.accessors.last().unwrap();
                if !container.set(&token.value, assigned_value.clone()) {
                    return Err(reassignment.ident.accessors[0]
                        .loc
                        .error(&format!("Failed to set property '{}'", &token.value)));
                }
                Ok(assigned_value)
            }
            Expression::String(string) => Ok(Expression::String(string.clone())),
            Expression::Touple(value) => {
                if value.len() == 1 {
                    return Ok(value[0].clone().eval(ctx)?);
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
                if let Some(value) = ctx.scope.get(&var.ident) {
                    return Ok(value);
                }
                var.not_defined_err()
            }
            Expression::FuncExpr(func_expr) => Ok(Expression::Closure(Box::new(Closure {
                func_expr: func_expr.clone(),
                scope: Rc::clone(&ctx.scope),
            }))),
            Expression::ForExpr(for_expr) => {
                let ident = &for_expr.identifier;
                let start = for_expr.start.as_int(&ident.loc, ctx)?;
                let end = for_expr.end.as_int(&ident.loc, ctx)?;
                let body = &for_expr.body;

                let mut last = Expression::Null;

                for i in start..end {
                    let child_ctx = FunCtx {
                        scope: Rc::new(Scope::create(&ctx.scope)),
                        output: Rc::clone(&ctx.output),
                        filename: ctx.filename.clone(),
                    };
                    child_ctx
                        .scope
                        .assign(ident.ident.clone(), Expression::Int(i));
                    last = eval_expr_list(&body, &child_ctx)?;
                }

                return Ok(last);
            }
            Expression::WhileExpr(for_expr) => {
                let condition = &for_expr.condition;
                let body = &for_expr.body;

                let mut last = Expression::Null;

                while condition.eval(ctx)?.as_bool()? {
                    let child_ctx = FunCtx {
                        scope: Rc::new(Scope::create(&ctx.scope)),
                        output: Rc::clone(&ctx.output),
                        filename: ctx.filename.clone(),
                    };
                    last = eval_expr_list(&body, &child_ctx)?;
                }

                return Ok(last);
            }
            Expression::IfExpr(if_expr) => {
                let condition = if_expr.condition.eager_eval(ctx)?.as_bool()?;
                if condition {
                    eval_expr_and_call_returned_block(&if_expr.then_expr, ctx)
                } else if let Some(else_expr) = &if_expr.else_expr {
                    eval_expr_and_call_returned_block(&else_expr, ctx)
                } else {
                    Ok(Expression::Null)
                }
            }
            Expression::Return(inner) => {
                let inner = inner.eval(ctx)?;
                return Ok(Expression::Return(Box::new(inner)));
            }
            Expression::Bool(value) => Ok(Expression::Bool(value.clone())),
            Expression::Null => Ok(Expression::Null),
            Expression::List(list) => Ok(Expression::List(list.eval(ctx)?)),
            Expression::Access(access) => access
                .target
                .eval(ctx)?
                .get_by_key_and_loc(&access.key, &access.loc),
            Expression::ImportExpr(import_expr) => import_expr.eval(ctx),
            _ => {
                todo!("Failed to eval expression {:?}", self);
            }
        }
    }

    pub fn as_string<W: Write>(&self, loc: &Loc, ctx: &FunCtx<W>) -> Result<String, LocError> {
        match self {
            Expression::String(string) => Ok(string.clone()),
            Expression::Int(int) => Ok(int.to_string()),
            Expression::Touple(value) => {
                let mut result = String::new();
                for expr in value {
                    let item = expr.eval(ctx);
                    match item {
                        Ok(item) => match item.as_string(loc, ctx) {
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
            Expression::Lazy(lazy_expr) => lazy_expr.eval(ctx)?.as_string(loc, ctx),
            Expression::Null => Ok(String::from("null")),
            Expression::Return(expression) => expression.as_string(loc, ctx),
            Expression::List(list) => list.as_string(ctx),
            // TODO: Split Expression and SimpleExpression
            Expression::FuncCall(_func_call) => todo!(),
            Expression::Access(_) => todo!(),
            Expression::VarDecl(_var_decl) => todo!(),
            Expression::ReAssignment(_re_assignment) => todo!(),
            Expression::Variable(var) => ctx
                .scope
                .get(&var.ident)
                .ok_or(var.loc.error("Could not resolve variable"))?
                .as_string(&var.loc, ctx),
            Expression::FuncExpr(_func_expr) => todo!(),
            Expression::Closure(_closure) => todo!(),
            Expression::IfExpr(_if_expr) => todo!(),
            Expression::Block(_func_call) => todo!(),
            Expression::Map(_map) => todo!(),
            Expression::ForExpr(_) => todo!(),
            Expression::WhileExpr(_) => todo!(),
            Expression::ImportExpr(_) => todo!(),
            Expression::BuiltinFuncCall(_) => todo!(),
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

impl List {
    pub fn eval<W: Write>(&self, ctx: &FunCtx<W>) -> Result<List, LocError> {
        let mut items = vec![];

        for expr in &*self.items.borrow() {
            items.push(expr.eval(ctx)?);
        }

        Ok(List {
            loc: self.loc.clone(),
            items: Rc::new(RefCell::new(items)),
        })
    }

    pub fn as_string<W: Write>(&self, ctx: &FunCtx<W>) -> Result<String, LocError> {
        let mut stringified_items = vec![];

        for item in &*self.items.borrow() {
            stringified_items.push(item.as_string(&self.loc, ctx)?);
        }

        return Ok(format!("[{}]", join(", ", stringified_items)));
    }
}

fn expressions_to_debug_str(expressions: &Vec<Expression>) -> String {
    join(", ", expressions.iter().map(|e| e.debug_str()).collect())
}

impl List {
    pub fn get(&self, key: &str, loc: &Loc) -> Option<Expression> {
        match key.as_ref() {
            "push" => Some(Expression::BuiltinFuncCall(Builtin {
                loc: loc.clone(),
                name: BuiltinName::ListPush,
                this: Box::new(Expression::List(self.clone())),
            })),
            "len" => Some(Expression::BuiltinFuncCall(Builtin {
                loc: loc.clone(),
                name: BuiltinName::ListLen,
                this: Box::new(Expression::List(self.clone())),
            })),
            _ => None,
        }
    }
}

impl Builtin {
    pub fn as_string(&self) -> String {
        match self.name {
            BuiltinName::ListPush => "list.push".to_string(),
            BuiltinName::ListLen => "list.len".to_string(),
        }
    }
}

