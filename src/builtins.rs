use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

use crate::{Expression, FuncCall, LocError, Scope};

pub fn fun_add<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let args = func_call.arg.as_vec();

    let mut sum = 0i64;

    for arg in args {
        sum += arg.as_int(&func_call.ident.loc(), output, scope)?;
    }

    Ok(Expression::Int(sum))
}

pub fn fun_sub<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let args = func_call.arg.eager_eval(output, scope)?.as_vec();

    let mut sum = args.get(0)
        .ok_or(func_call.ident.error("At least one argument is required"))?
        .as_int(&func_call.ident.loc(), output, scope)?;

    for i in 1..args.len() {
        sum -= args[i].as_int(&func_call.ident.loc(), output, scope)?;
    }

    Ok(Expression::Int(sum))
}

pub fn fun_eq<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    expr: &Expression,
) -> Result<Expression, LocError> {
    let values = expr.eval(output, scope)?.as_vec();

    for i in 1..values.len() {
        let a = values[i].eager_eval(output, scope)?;
        let b = values[i - 1].eager_eval(output, scope)?;
        if a != b {
            return Ok(Expression::Bool(false));
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_str<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    Ok(Expression::String(
        func_call
            .arg
            .eval(output, scope)?
            .eval(output, scope)?
            .as_string(&func_call.ident.loc(), output, scope)?,
    ))
}

pub fn fun_not<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    Ok(Expression::Bool(
        !func_call
            .arg
            .eval(output, scope)?
            .eval(output, scope)?
            .as_bool()?,
    ))
}

pub fn fun_lte<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let values = func_call.arg.eval(output, scope)?.as_vec();

    if values.len() > 0 {
        let mut first = values[0].eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;
        for i in 1..values.len() {
            let second = values[i].eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;
            if !(first <= second) {
                return Ok(Expression::Bool(false));
            }
            first = second;
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_lt<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let values = func_call.arg.eval(output, scope)?.as_vec();

    if values.len() > 0 {
        let mut first = values[0].eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;
        for i in 1..values.len() {
            let second = values[i].eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;
            if !(first < second) {
                return Ok(Expression::Bool(false));
            }
            first = second;
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_gte<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let values = func_call.arg.eval(output, scope)?.as_vec();

    if values.len() > 0 {
        let mut first = values[0].eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;
        for i in 1..values.len() {
            let second = values[i].eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;
            if !(first >= second) {
                return Ok(Expression::Bool(false));
            }
            first = second;
        }
    }

    Ok(Expression::Bool(true))
}

pub fn fun_create_map<W: Write>(
    _scope: &Rc<Scope>,
    _output: &Rc<RefCell<W>>,
    _expr: &Expression,
) -> Result<Expression, LocError> {
    Ok(Expression::Map(Rc::new(RefCell::new(HashMap::new()))))
}

pub fn fun_print<W: Write>(
    func_call: &FuncCall,
    output: &Rc<RefCell<W>>,
    scope: &Rc<Scope>,
) -> Result<Expression, LocError> {
    let value = func_call.arg.eval(output, scope)?;
    let result = value.as_string(&func_call.ident.loc(), output, scope)?;
    write!(output.borrow_mut(), "{}", &result).unwrap();
    Ok(Expression::Null)
}

pub fn fun_println<W: Write>(
    func_call: &FuncCall,
    output: &Rc<RefCell<W>>,
    scope: &Rc<Scope>,
) -> Result<Expression, LocError> {
    let value = func_call.arg.eval(output, scope)?;
    let result = value.as_string(&func_call.ident.loc(), output, scope)?;
    writeln!(output.borrow_mut(), "{}", &result).unwrap();
    Ok(Expression::Null)
}

pub fn fun_or<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let expressions = func_call.arg.eval(output, scope)?.as_vec();

    if expressions.len() < 1 {
        return Err(func_call.ident.error("or requires at least one argument"));
    }

    for expression in expressions {
        let value = expression.eval(output, scope)?;

        if value.as_bool()? {
            return Ok(Expression::Bool(true));
        }
    }

    return Ok(Expression::Bool(false));
}

pub fn fun_and<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    println!("FUN OR: {}", func_call.arg.debug_str());

    let expressions = func_call.arg.eval(output, scope)?.as_vec();

    if expressions.len() < 1 {
        return Err(func_call.ident.error("or requires at least one argument"));
    }

    for expression in expressions {
        let value = expression.eval(output, scope)?;

        println!("value: {}", value.debug_str());

        if !value.as_bool()? {
            return Ok(Expression::Bool(false));
        }
    }

    return Ok(Expression::Bool(true));
}

pub fn fun_modulo<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let arg = func_call.arg.clone();

    let expr = arg.eval(output, scope)?;

    let (first, second) = expr
        .require_two_touple()
        .ok_or(func_call.ident.error("Expected two arguments"))?;

    let first = first.eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;
    let second = second.eval(output, scope)?.as_int(&func_call.ident.loc(), output, scope)?;

    return Ok(Expression::Int(first % second));
}

pub fn fun_type<W: Write>(
    scope: &Rc<Scope>,
    output: &Rc<RefCell<W>>,
    func_call: &FuncCall,
) -> Result<Expression, LocError> {
    let expr = func_call.arg.eval(output, scope)?;
    Ok(Expression::String(expr.type_str().to_string()))
}

pub fn fun_etype(
    expr: &Expression
) -> Result<Expression, LocError> {
    Ok(Expression::String(expr.type_str().to_string()))
}
