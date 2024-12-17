use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

use crate::{context::FunContext, Expression, FuncCall, LazyExpression, LocError, Scope};

pub fn fun_add<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let args = func_call.arg.as_vec();

    let mut sum = 0i64;

    for arg in args {
        sum += arg.as_int(&func_call.ident.loc(), ctx)?;
    }

    Ok(Expression::Int(sum))
}

pub fn fun_sub<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let args = func_call.arg.eager_eval(ctx)?.as_vec();

    let mut sum = args
        .get(0)
        .ok_or(func_call.ident.error("At least one argument is required"))?
        .as_int(&func_call.ident.loc(), ctx)?;

    for i in 1..args.len() {
        sum -= args[i].as_int(&func_call.ident.loc(), ctx)?;
    }

    Ok(Expression::Int(sum))
}

pub fn fun_eq<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let values = func_call.arg.eval(ctx)?.as_vec();

    for i in 1..values.len() {
        let a = values[i].eager_eval(ctx)?;
        let b = values[i - 1].eager_eval(ctx)?;
        if a != b {
            return Ok(Expression::Bool(false));
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_str<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    Ok(Expression::String(
        func_call
            .arg
            .eval(ctx)?
            .eval(ctx)?
            .as_string(&func_call.ident.loc(), ctx)?,
    ))
}

pub fn fun_not<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    Ok(Expression::Bool(
        !func_call
            .arg
            .eval(ctx)?
            .eval(ctx)?
            .as_bool()?,
    ))
}

pub fn fun_lte<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let values = func_call.arg.eval(ctx)?.as_vec();

    if values.len() > 0 {
        let mut first =
            values[0]
                .eval(ctx)?
                .as_int(&func_call.ident.loc(), ctx)?;
        for i in 1..values.len() {
            let second =
                values[i]
                    .eval(ctx)?
                    .as_int(&func_call.ident.loc(), ctx)?;
            if !(first <= second) {
                return Ok(Expression::Bool(false));
            }
            first = second;
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_lt<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let values = func_call.arg.eval(ctx)?.as_vec();

    if values.len() > 0 {
        let mut first =
            values[0]
                .eval(ctx)?
                .as_int(&func_call.ident.loc(), ctx)?;
        for i in 1..values.len() {
            let second =
                values[i]
                    .eval(ctx)?
                    .as_int(&func_call.ident.loc(), ctx)?;
            if !(first < second) {
                return Ok(Expression::Bool(false));
            }
            first = second;
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_gte<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let values = func_call.arg.eval(ctx)?.as_vec();

    if values.len() > 0 {
        let mut first =
            values[0]
                .eval(ctx)?
                .as_int(&func_call.ident.loc(), ctx)?;
        for i in 1..values.len() {
            let second =
                values[i]
                    .eval(ctx)?
                    .as_int(&func_call.ident.loc(), ctx)?;
            if !(first >= second) {
                return Ok(Expression::Bool(false));
            }
            first = second;
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_create_map() -> Result<Expression, LocError> {
    Ok(Expression::Map(Rc::new(RefCell::new(HashMap::new()))))
}

pub fn fun_lazy<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    Ok(Expression::Lazy(Box::new(LazyExpression {
        expr: *func_call.arg.clone(),
        scope: Rc::clone(&ctx.scope),
    })))
}

pub fn fun_print<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let value = func_call.arg.eval(ctx)?;
    let result = value.as_string(&func_call.ident.loc(), ctx)?;
    write!(ctx.output.borrow_mut(), "{}", &result).unwrap();
    Ok(Expression::Null)
}

pub fn fun_println<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let value = func_call.arg.eval(ctx)?;
    let result = value.as_string(&func_call.ident.loc(), ctx)?;
    writeln!(ctx.output.borrow_mut(), "{}", &result).unwrap();
    Ok(Expression::Null)
}

pub fn fun_or<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let expressions = func_call.arg.eval(ctx)?.as_vec();

    if expressions.len() < 1 {
        return Err(func_call.ident.error("or requires at least one argument"));
    }

    for expression in expressions {
        let value = expression.eval(ctx)?;

        if value.as_bool()? {
            return Ok(Expression::Bool(true));
        }
    }

    return Ok(Expression::Bool(false));
}

pub fn fun_and<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    println!("FUN OR: {}", func_call.arg.debug_str());

    let expressions = func_call.arg.eval(ctx)?.as_vec();

    if expressions.len() < 1 {
        return Err(func_call.ident.error("or requires at least one argument"));
    }

    for expression in expressions {
        let value = expression.eval(ctx)?;

        println!("value: {}", value.debug_str());

        if !value.as_bool()? {
            return Ok(Expression::Bool(false));
        }
    }

    return Ok(Expression::Bool(true));
}

pub fn fun_modulo<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let arg = func_call.arg.clone();

    let expr = arg.eval(ctx)?;

    let (first, second) = expr
        .require_two_touple()
        .ok_or(func_call.ident.error("Expected two arguments"))?;

    let first = first
        .eval(ctx)?
        .as_int(&func_call.ident.loc(), ctx)?;
    let second = second
        .eval(ctx)?
        .as_int(&func_call.ident.loc(), ctx)?;

    return Ok(Expression::Int(first % second));
}

pub fn fun_type<W: Write>(
    ctx: &FunContext<W>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let expr = func_call.arg.eval(ctx)?;
    Ok(Expression::String(expr.type_str().to_string()))
}

pub fn fun_etype(func_call: &FuncCall) -> Result<Expression, LocError> {
    Ok(Expression::String(func_call.arg.type_str().to_string()))
}

pub fn fun_core_env() -> Expression {
    let mut env_map = HashMap::new();

    for (key, val) in std::env::vars_os() {
        // Use pattern bindings instead of testing .is_some() followed by .unwrap()
        if let (Ok(k), Ok(v)) = (key.into_string(), val.into_string()) {
            env_map.insert(k, Expression::String(v));
        }
    }

    Expression::Map(Rc::new(RefCell::new(env_map)))
}

pub fn fun_in<W: Write>(ctx: &FunContext<W>, func_call: &FuncCall) -> Result<Expression, LocError> {
    let args = func_call.arg.as_vec();

    match args.len() {
        2 => {
            func_call.arg.eval(ctx)
        },
        _ => {
            Err(func_call.ident.loc().error("'in' requires two arguments"))
        },
    }
}

pub fn fun_core_http() -> Expression {
    // TODO
    Expression::Null
}

pub fn fun_core_module() -> Scope {
    let scope = Scope::new();
    scope.assign("env".to_string(), fun_core_env());
    scope.assign("http".to_string(), fun_core_http());
    scope
}
