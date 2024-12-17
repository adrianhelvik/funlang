use crate::context::FunContext;
use crate::scope::Scope;
use crate::types::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
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

    let ctx = FunContext {
        scope,
        output: Rc::clone(output),
        filename: filename.to_string(),
    };

    eval_expr_list(&program.expressions, &ctx)?;

    Ok(())
}

pub fn eval_expr_list<W: Write>(
    expressions: &Vec<Expression>,
    ctx: &FunContext<W>,
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
    ctx: &FunContext<W>,
) -> Result<Expression, LocError> {
    let res = expr.eval(ctx)?;

    match res {
        Expression::Closure(closure) => {
            let child_scope = Scope::create(&ctx.scope);
            child_scope.assign(
                String::from("__block__"),
                Expression::FuncExpr(closure.func_expr),
            );
            let child_ctx = FunContext {
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
            })
            .eval(&child_ctx)?;
            Ok(value)
        }
        other => Ok(other),
    }
}

pub fn lookup(scope: &Rc<Scope>, ident: &Identifier) -> Result<(Expression, Loc), LocError> {
    let mut value = scope
        .get(&ident.accessors[0].value)
        .ok_or_else(|| ident.accessors[0].loc.error("Failed to look up variable"))?;

    for i in 1..ident.accessors.len() {
        let token = &ident.accessors[i];
        value = value.get_by_key_token(&token)?;
    }

    Ok((value, ident.accessors.last().unwrap().loc.clone()))
}

pub fn call_func_expr<W: Write>(
    func_call: &FuncCall,
    ctx: &FunContext<W>,
) -> Result<Expression, LocError> {
    let (expression, loc) = lookup(&ctx.scope, &func_call.ident)?;

    match expression {
        Expression::Closure(closure) => call_func(
            &closure.func_expr,
            func_call,
            ctx,
            &FunContext {
                scope: Rc::clone(&closure.scope),
                output: Rc::clone(&ctx.output),
                filename: ctx.filename.clone(),
            }
        ),
        Expression::FuncExpr(func_expr) => call_func(&func_expr, &func_call, ctx, ctx),
        Expression::Map(map) => call_map(&loc, &map, func_call, ctx),
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
    ctx: &FunContext<W>,
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
    ctx: &FunContext<W>,
    enclosing_ctx: &FunContext<W>,
) -> Result<Expression, LocError> {
    let function_ctx = FunContext {
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
