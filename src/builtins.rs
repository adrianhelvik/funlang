use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

use crate::{context::FunCtx, interpreter::InterpreterResult, scope::Scope, Expression, FuncCall, LazyExpression, Loc};

/*
pub fn builtins() {

                match &func_call.target.expr {
                    Expression::Variable(var) => match var.ident.as_ref() {
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
                        "gt" => return fun_gt(ctx, &*func_call),
                        "or" => return fun_or(ctx, &*func_call),
                        "type" => return fun_type(ctx, &func_call),
                        "etype" => return fun_etype(&func_call),
                        "and" => return fun_and(ctx, &*func_call),
                        "modulo" => return fun_modulo(ctx, &*func_call),
                        "Map" => return fun_create_map(&*func_call),
                        "lazy" => return fun_lazy(ctx, &func_call),
                        "in" => return fun_in(ctx, func_call),
                        _ => {}
                    },
                    _ => {},
                }

}
*/

pub fn fun_add<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let args = func_call.arg.as_vec();

    let mut sum = 0i64;

    for arg in args {
        sum += arg.as_int(ctx)?;
    }

    Ok(func_call.loc().wrap(Expression::Int(sum)))
}

pub fn fun_sub<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let args = func_call.arg.eager_eval(ctx)?.as_vec();

    let mut sum = args
        .get(0)
        .ok_or(func_call.target.loc.error("At least one argument is required"))?
        .as_int(ctx)?;

    for i in 1..args.len() {
        sum -= args[i].as_int(ctx)?;
    }

    Ok(func_call.target.loc.wrap(Expression::Int(sum)))
}

pub fn fun_eq<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let values = func_call.arg.eval(ctx)?.as_vec();

    for i in 1..values.len() {
        let a = values[i].eager_eval(ctx)?;
        let b = values[i - 1].eager_eval(ctx)?;
        if a.expr != b.expr {
            return Ok(func_call.loc().wrap(Expression::Bool(false)));
        }
    }

    Ok(func_call.expr(Expression::Bool(true)))
}

pub fn fun_str<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    Ok(func_call.expr(Expression::String(
        func_call
            .arg
            .eval(ctx)?
            .as_string(ctx)?,
    )))
}

pub fn fun_not<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    Ok(func_call.expr(Expression::Bool(
        !func_call
            .arg
            .eval(ctx)?
            .as_bool()?,
    )))
}

pub fn fun_lte<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let values = func_call.arg.eval(ctx)?.as_vec();

    if values.len() > 0 {
        let mut first =
            values[0]
                .eval(ctx)?
                .as_int(ctx)?;
        for i in 1..values.len() {
            let second =
                values[i]
                    .eval(ctx)?
                    .as_int(ctx)?;
            if !(first <= second) {
                return Ok(func_call.expr(Expression::Bool(false)));
            }
            first = second;
        }
    }

    Ok(func_call.expr(Expression::Bool(true)))
}

pub fn fun_lt<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let values = func_call.arg.eval(ctx)?.as_vec();

    if values.len() > 0 {
        let mut first =
            values[0]
                .eval(ctx)?
                .as_int(ctx)?;
        for i in 1..values.len() {
            let second =
                values[i]
                    .eval(ctx)?
                    .as_int(ctx)?;
            if !(first < second) {
                return Ok(func_call.expr(Expression::Bool(false)));
            }
            first = second;
        }
    }

    Ok(func_call.expr(Expression::Bool(true)))
}

pub fn fun_gte<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let values = func_call.arg.eval(ctx)?.as_vec();

    if values.len() > 0 {
        let mut first =
            values[0]
                .eval(ctx)?
                .as_int(ctx)?;
        for i in 1..values.len() {
            let second =
                values[i]
                    .eval(ctx)?
                    .as_int(ctx)?;
            if !(first >= second) {
                return Ok(func_call.expr(Expression::Bool(false)));
            }
            first = second;
        }
    }

    Ok(func_call.expr(Expression::Bool(true)))
}

pub fn fun_gt<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let values = func_call.arg.eval(ctx)?.as_vec();

    if values.len() > 0 {
        let mut first =
            values[0]
                .eval(ctx)?
                .as_int(ctx)?;
        for i in 1..values.len() {
            let second =
                values[i]
                    .eval(ctx)?
                    .as_int(ctx)?;
            if !(first > second) {
                return Ok(func_call.expr(Expression::Bool(false)));
            }
            first = second;
        }
    }

    Ok(func_call.expr(Expression::Bool(true)))
}

pub fn fun_create_map<W: Write>(_ctx: &FunCtx<W>, func_call: &FuncCall) -> InterpreterResult {
    Ok(func_call.expr(Expression::Map(Rc::new(RefCell::new(HashMap::new())))))
}

pub fn fun_lazy<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    Ok(func_call.expr(Expression::Lazy(Box::new(LazyExpression {
        expr: *func_call.arg.clone(),
        scope: Rc::clone(&ctx.scope),
    }))))
}

pub fn fun_print<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let value = func_call.arg.eval(ctx)?;
    let result = value.as_string(ctx)?;
    ctx.write(&result);
    Ok(func_call.expr(Expression::Null))
}

pub fn fun_println<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let value = func_call.arg.eval(ctx)?;
    let result = value.as_string(ctx)?;
    ctx.writeln(&result);
    Ok(func_call.expr(Expression::Null))
}

pub fn fun_or<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let expressions = func_call.arg.eval(ctx)?.as_vec();

    if expressions.len() < 1 {
        return Err(func_call.error("or requires at least one argument"));
    }

    for expression in expressions {
        let value = expression.eval(ctx)?;

        if value.as_bool()? {
            return Ok(func_call.expr(Expression::Bool(true)));
        }
    }

    return Ok(func_call.expr(Expression::Bool(false)));
}

pub fn fun_and<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let expressions = func_call.arg.eval(ctx)?.as_vec();

    if expressions.len() < 1 {
        return Err(func_call.error("or requires at least one argument"));
    }

    for expression in expressions {
        let value = expression.eval(ctx)?;

        if !value.as_bool()? {
            return Ok(func_call.expr(Expression::Bool(false)));
        }
    }

    return Ok(func_call.expr(Expression::Bool(true)));
}

pub fn fun_modulo<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let arg = func_call.arg.clone();

    let expr = arg.eval(ctx)?;

    let (first, second) = expr.require_two_arguments()?;

    let first = first
        .eval(ctx)?
        .as_int(ctx)?;
    let second = second
        .eval(ctx)?
        .as_int(ctx)?;

    return Ok(func_call.expr(Expression::Int(first % second)));
}

pub fn fun_type<W: Write>(
    ctx: &FunCtx<W>,
    func_call: &FuncCall,
) -> InterpreterResult {
    let expr = func_call.arg.eval(ctx)?;
    Ok(func_call.expr(Expression::String(expr.type_str().to_string())))
}

pub fn fun_etype(func_call: &FuncCall) -> InterpreterResult {
    Ok(func_call.expr(Expression::String(func_call.arg.type_str().to_string())))
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

pub fn fun_in<W: Write>(ctx: &FunCtx<W>, func_call: &FuncCall) -> InterpreterResult {
    let args = func_call.arg.as_vec();

    match args.len() {
        2 => {
            func_call.arg.eval(ctx)
        },
        _ => {
            Err(func_call.error("'in' requires two arguments"))
        },
    }
}

pub fn fun_list_len<W: Write>(_ctx: &FunCtx<W>, func_call: &FuncCall) -> InterpreterResult {
    if let Expression::List(list) = &(*func_call.this).expr {
        Ok(func_call.expr(Expression::Int(list.items.borrow().len().try_into().unwrap())))
    } else {
        Err(func_call.error(&format!("Attempted to call list.push on '{}'", func_call.this.type_str())))
    }
}

pub fn fun_list_push<W: Write>(ctx: &FunCtx<W>, func_call: &FuncCall) -> InterpreterResult {
    let args = func_call.arg.as_vec();

    if args.len() == 0 {
        return Err(func_call.error("You must specify the item to push"));
    }

    if args.len() > 1 {
        return Err(func_call.error("This method only accepts one argument"));
    }

    if let Expression::List(list) = &(*func_call.this).expr {
        let value = args[0].eval(ctx)?;
        {
            let mut values = list.items.borrow_mut();
            values.push(value);
        }
        Ok(func_call.expr(Expression::List(list.clone())))
    } else {
        Err(func_call.error(&format!("Attempted to call list.push on '{}'", func_call.this.type_str())))
    }
}

pub fn fun_prelude() -> Rc<Scope> {
    let scope = Scope::new();

    // TODO: Assign prelude variables here

    Rc::new(scope)
}

pub fn fun_core_http() -> Expression {
    // TODO
    Expression::Null
}

pub fn fun_core_module() -> Scope {
    let loc = Loc::new(1, 1);
    let scope = Scope::new();
    scope.assign("env".to_string(), loc.wrap(fun_core_env()));
    scope.assign("http".to_string(), loc.wrap(fun_core_http()));
    scope
}
