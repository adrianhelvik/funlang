use crate::types::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

pub fn eval_program<W: Write>(program: &Program, output: &Rc<RefCell<W>>) -> Result<(), LocError> {
    let scope = Rc::new(Scope::new());

    eval_expr_list(&program.expressions, output, &scope)?;

    Ok(())
}

pub fn eval_expr_list<W: Write>(
    expressions: &Vec<Expression>,
    output: &Rc<RefCell<W>>,
    scope: &Rc<Scope>,
) -> Result<Expression, LocError> {
    let mut value = Expression::Null;

    for expr in expressions {
        value = expr.eval(output, scope)?;

        if let Expression::Return(result) = value {
            return Ok(Expression::Return(result));
        }
    }

    Ok(value)
}

pub fn eval_expr_and_call_returned_block<W: Write>(
    expr: &Expression,
    output: &Rc<RefCell<W>>,
    scope: &Rc<Scope>,
) -> Result<Expression, LocError> {
    let res = expr.eval(output, scope)?;

    match res {
        Expression::Closure(closure) => {
            let child_scope = Scope::create(scope);
            child_scope.assign(
                String::from("__block__"),
                Expression::FuncExpr(closure.func_expr),
            );
            let child_scope = Rc::new(child_scope);
            // TODO: Consider if I want this
            let value = Expression::Block(FuncCall {
                ident: Identifier { accessors: vec![Token { value: "__block__".to_string(), loc: Loc::new(0, 0) }] },
                arg: Box::new(Expression::Null),
            })
            .eval(output, &child_scope)?;
            Ok(value)
        }
        val => {
            panic!("Failed to call {:#?}", val);
        }
    }
}

pub fn lookup(scope: &Rc<Scope>, ident: &Identifier) -> Result<(Expression, Loc), LocError> {
    let mut value = scope.get(&ident.accessors[0].value)
        .ok_or_else(|| ident.accessors[0].loc.error("Failed to look up"))?;

    for i in 1..ident.accessors.len() {
        let token = &ident.accessors[i];
        value = value.get_by_key(&token.value)
            .ok_or_else(|| token.loc.error("Failed to look up"))?
    }

    Ok((value, ident.accessors.last().unwrap().loc.clone()))
}

pub fn call_func_expr<W: Write>(
    func_call: &FuncCall,
    output: &Rc<RefCell<W>>,
    scope: &Rc<Scope>,
) -> Result<Expression, LocError> {
    let (expression, loc) = lookup(scope, &func_call.ident)?;

    match expression {
        Expression::Closure(closure) => call_func(
            &closure.func_expr,
            &func_call,
            output,
            &closure.scope,
            scope,
        ),
        Expression::FuncExpr(func_expr) => {
            call_func(&func_expr, &func_call, output, scope, scope)
        }
        Expression::Map(map) => call_map(&loc, &map, func_call, output, scope),
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
    output: &Rc<RefCell<W>>,
    scope: &Rc<Scope>,
) -> Result<Expression, LocError> {
    let args = func_call.arg.as_vec();

    match args.len() {
        1 => {
            let key =
                args[0]
                    .eval(output, scope)?
                    .as_string(loc, output, scope)?;
            let val = match map.borrow().get(&key) {
                Some(expr) => expr.clone(),
                None => Expression::Null,
            };
            Ok(val)
        }
        2 => {
            let key =
                args[0]
                    .eval(output, scope)?
                    .as_string(loc, output, scope)?;
            let val = args[1].eval(output, scope)?;
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
    output: &Rc<RefCell<W>>,
    scope: &Rc<Scope>,
    parent_scope: &Rc<Scope>,
) -> Result<Expression, LocError> {
    let function_scope = Rc::new(Scope::create(scope));

    let passed_in_args = func_call.arg.as_vec();

    for (i, ident) in func_expr.args.iter().enumerate() {
        if let Some(arg) = passed_in_args.get(i) {
            let value = arg.eval(output, parent_scope)?;
            function_scope.assign(ident.clone(), value);
        } else {
            function_scope.assign(ident.clone(), Expression::Null);
        }
    }

    let result = eval_expr_list(&func_expr.expressions, output, &function_scope)?;

    Ok(result)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{lexer::lex, parser::parse};

    use super::*;

    fn eval_ast(ast: &Program) -> String {
        let output = Rc::new(RefCell::new(Vec::new()));

        if let Err(err) = eval_program(ast, &output) {
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
