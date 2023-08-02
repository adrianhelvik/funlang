use crate::types::*;
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

pub fn eval_program<W: Write>(program: Program, output: Rc<RefCell<W>>) -> Result<(), LocError> {
    let scope = Rc::new(Scope::new());

    for expr in program.expressions {
        match eval_expr(expr, Rc::clone(&output), Rc::clone(&scope)) {
            Ok(_) => {}
            Err(err) => {
                return Err(err);
            }
        }
    }

    Ok(())
}

fn to_int(expr: Expression) -> i64 {
    match expr {
        Expression::Int(int) => int,
        _ => {
            panic!("Failed to convert {:#?} to Int", expr);
        }
    }
}

fn eval_expr_and_call_returned_block<W: Write>(
    expr: Expression,
    output: Rc<RefCell<W>>,
    scope: Rc<Scope>,
) -> Result<Expression, LocError> {
    match eval_expr(expr, Rc::clone(&output), Rc::clone(&scope)) {
        Ok(res) => match res {
            Expression::Closure(closure) => {
                let child_scope = Scope::create(scope);
                child_scope.assign(
                    String::from("__block__"),
                    Expression::FuncExpr(closure.func_expr),
                );
                let child_scope = Rc::new(child_scope);
                match eval_expr(
                    Expression::FuncCall(FuncCall {
                        ident: Variable {
                            ident: String::from("__block__"),
                            loc: Loc::new(0, 0),
                        },
                        arg: Box::new(Expression::Null),
                    }),
                    Rc::clone(&output),
                    child_scope,
                ) {
                    Ok(res) => Ok(res),
                    Err(err) => Err(err),
                }
            }
            val => {
                panic!("Failed to call {:#?}", val);
            }
        },
        Err(err) => {
            return Err(err);
        }
    }
}

pub fn fun_add<W: Write>(
    scope: Rc<Scope>,
    output: Rc<RefCell<W>>,
    expr: Expression,
) -> Result<Expression, LocError> {
    match eval_expr(expr.clone(), Rc::clone(&output), Rc::clone(&scope)) {
        Ok(res) => match res {
            Expression::Touple(expressions) => {
                let mut result = 0;
                for expr in expressions {
                    match eval_expr(expr, Rc::clone(&output), Rc::clone(&scope)) {
                        Ok(value) => {
                            result += to_int(value);
                        }
                        Err(err) => {
                            return Err(err);
                        }
                    }
                }
                Ok(Expression::Int(result))
            }
            Expression::Int(int) => Ok(Expression::Int(int)),
            _ => {
                panic!("Failed to add {:#?}", expr);
            }
        },
        Err(err) => {
            return Err(err);
        }
    }
}

pub fn eval_expr<W: Write>(
    expr: Expression,
    output: Rc<RefCell<W>>,
    scope: Rc<Scope>,
) -> Result<Expression, LocError> {
    match expr {
        Expression::FuncCall(func_call) => {
            if func_call.ident.ident == "print" {
                let value = eval_expr(*func_call.arg, Rc::clone(&output), Rc::clone(&scope));
                match value {
                    Ok(value) => {
                        let result = expr_to_str(value, Rc::clone(&output), scope);
                        match result {
                            Ok(res) => {
                                write!(output.borrow_mut(), "{}", &res).unwrap();
                                Ok(Expression::Null)
                            }
                            Err(err) => Err(err),
                        }
                    }
                    Err(err) => Err(err),
                }
            } else if func_call.ident.ident == "println" {
                let value = eval_expr(*func_call.arg, Rc::clone(&output), Rc::clone(&scope));
                match value {
                    Ok(value) => {
                        let result = expr_to_str(value, Rc::clone(&output), Rc::clone(&scope));
                        match result {
                            Ok(result) => {
                                writeln!(output.borrow_mut(), "{}", &result).unwrap();
                                Ok(Expression::Null)
                            }
                            Err(err) => Err(err),
                        }
                    }
                    Err(err) => Err(err),
                }
            } else if func_call.ident.ident == "add" {
                fun_add(scope, output, *func_call.arg)
            } else if func_call.ident.ident == "ret" || func_call.ident.ident == "return" {
                match eval_expr(*func_call.arg, output, Rc::clone(&scope)) {
                    Ok(val) => Ok(Expression::Return(Box::new(val))),
                    Err(err) => Err(err),
                }
            } else {
                if let Some(expression) = scope.get(func_call.ident.ident.clone()) {
                    if let Expression::Closure(closure) = expression {
                        call_func(closure.func_expr, func_call, output, closure.scope)
                    } else if let Expression::FuncExpr(func_expr) = expression {
                        call_func(func_expr, func_call, output, scope)
                    } else {
                        panic!("{:#?} is not callable", expression);
                    }
                } else {
                    Err(LocError {
                        message: format!(
                            "Attempted to call {}, which is not defined",
                            func_call.ident.ident
                        ),
                        loc: func_call.ident.loc,
                    })
                }
            }
        }
        Expression::VarDecl(assignment) => {
            match eval_expr(assignment.expr, output, Rc::clone(&scope)) {
                Ok(value) => {
                    scope.assign(assignment.ident, value.clone());
                    Ok(value)
                }
                Err(e) => Err(e),
            }
        }
        Expression::ReAssignment(assignment) => match scope.get(assignment.ident.clone()) {
            Some(_) => match eval_expr(assignment.expr, output, Rc::clone(&scope)) {
                Ok(value) => {
                    scope.set(assignment.ident, value.clone());
                    Ok(value)
                }
                Err(err) => Err(err),
            },
            None => {
                panic!(
                    "Attempted to assign to undefined variable {}",
                    assignment.ident
                );
            }
        },
        Expression::String(string) => Ok(Expression::String(string)),
        Expression::Touple(value) => Ok(Expression::Touple(value)),
        Expression::Int(value) => Ok(Expression::Int(value)),
        Expression::Variable(var) => {
            if let Some(value) = scope.get(var.ident.clone()) {
                return Ok(value.clone());
            }
            Err(LocError {
                loc: var.loc,
                message: format!("Variable '{}' is not defined", var.ident),
            })
        }
        Expression::FuncExpr(func_expr) => Ok(Expression::Closure(Box::new(Closure {
            func_expr,
            scope: Rc::clone(&scope),
        }))),
        Expression::IfExpr(if_expr) => {
            let condition = eval_expr(if_expr.condition, Rc::clone(&output), Rc::clone(&scope));
            match condition {
                Ok(condition) => match condition {
                    Expression::Int(int) => {
                        if int == 1 {
                            eval_expr_and_call_returned_block(
                                if_expr.then_expr,
                                Rc::clone(&output),
                                Rc::clone(&scope),
                            )
                        } else if let Some(else_expr) = if_expr.else_expr {
                            eval_expr_and_call_returned_block(
                                else_expr,
                                Rc::clone(&output),
                                Rc::clone(&scope),
                            )
                        } else {
                            Ok(Expression::Null)
                        }
                    }
                    _ => {
                        panic!("Condition must be an int");
                    }
                },
                Err(err) => Err(err),
            }
        }
        _ => {
            panic!("Failed to eval expression {:?}", expr);
        }
    }
}

pub fn expr_to_str<W: Write>(
    expr: Expression,
    output: Rc<RefCell<W>>,
    scope: Rc<Scope>,
) -> Result<String, LocError> {
    match expr {
        Expression::String(string) => Ok(string),
        Expression::Int(int) => Ok(int.to_string()),
        Expression::Touple(value) => {
            let mut result = String::new();
            for expr in value {
                let item = eval_expr(expr, Rc::clone(&output), Rc::clone(&scope));
                match item {
                    Ok(item) => match expr_to_str(item, Rc::clone(&output), Rc::clone(&scope)) {
                        Ok(res) => {
                            result.push_str(&res);
                        }
                        Err(err) => {
                            return Err(err);
                        }
                    },
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
            Ok(result)
        }
        Expression::Null => Ok(String::from("null")),
        _ => {
            panic!("Failed to stringify expression {:#?}", expr);
        }
    }
}

fn call_func<W: Write>(
    func_expr: FuncExpr,
    func_call: FuncCall,
    output: Rc<RefCell<W>>,
    scope: Rc<Scope>,
) -> Result<Expression, LocError> {
    let child_scope = RefCell::new(Rc::new(Scope::create(scope)));
    match *func_call.arg {
        Expression::Touple(args) => {
            for (i, arg) in args.iter().enumerate() {
                let ident = func_expr.args[i].clone();
                let val = eval_expr(
                    arg.clone(),
                    Rc::clone(&output),
                    Rc::clone(&child_scope.borrow()),
                );
                match val {
                    Ok(val) => {
                        child_scope.borrow().assign(ident, val);
                    }
                    Err(err) => return Err(err),
                }
            }
        }
        _ => {
            if func_expr.args.len() >= 1 {
                let ident = func_expr.args[0].clone();
                let val = eval_expr(
                    *func_call.arg,
                    Rc::clone(&output),
                    Rc::clone(&child_scope.borrow()),
                );
                match val {
                    Ok(val) => {
                        child_scope.borrow().assign(ident, val);
                    }
                    Err(err) => return Err(err),
                }
            }
        }
    }
    for expr in func_expr.expressions.clone() {
        match expr {
            Expression::Return(expr) => {
                return eval_expr(*expr, output, Rc::clone(&*child_scope.borrow()));
            }
            _ => {
                let scope = Rc::clone(&child_scope.borrow());
                if let Err(err) = eval_expr(expr, Rc::clone(&output), scope) {
                    return Err(err);
                }
            }
        }
    }
    Ok(Expression::Null)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn you_can_update_a_variable() {
        let program = Program {
            expressions: vec![
                Expression::VarDecl(Box::new(VarDecl {
                    expr: Expression::Int(1),
                    ident: String::from("x"),
                })),
                Expression::ReAssignment(Box::new(ReAssignment {
                    expr: Expression::Int(2),
                    ident: String::from("x"),
                })),
                Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("println"),
                        loc: Loc::new(1, 1),
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "x",
                        Loc::new(1, 1),
                    )))),
                }),
            ],
        };

        let output = Rc::new(RefCell::new(Vec::new()));

        if let Err(err) = eval_program(program, Rc::clone(&output)) {
            panic!("Failed to eval program {:#?}", err);
        }

        let output = output.borrow().clone();

        assert_eq!(String::from_utf8(output).unwrap(), "2\n");
    }
}
