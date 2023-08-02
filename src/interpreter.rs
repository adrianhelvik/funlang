use crate::types::*;
use std::io::Write;
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval_program<W: Write>(program: Program, output: Rc<RefCell<W>>) {
    let scope = Rc::new(Scope::new());

    for expr in program.expressions {
        eval_expr(expr, Rc::clone(&output), Rc::clone(&scope));
    }
}

fn to_int(expr: Expression) -> i64 {
    match expr {
        Expression::Int(int) => int,
        _ => {
            panic!("Failed to convert {:#?} to Int", expr);
        }
    }
}

pub fn fun_add<W: Write>(scope: Rc<Scope>, output: Rc<RefCell<W>>, expr: Expression) -> i64 {
    match eval_expr(expr.clone(), Rc::clone(&output), Rc::clone(&scope)) {
        Expression::Touple(expressions) => {
            let mut result = 0;
            for expr in expressions {
                result += to_int(eval_expr(expr, Rc::clone(&output), Rc::clone(&scope)));
            }
            result
        }
        Expression::Int(int) => int,
        _ => {
            panic!("Failed to add {:#?}", expr);
        }
    }
}

pub fn eval_expr<W: Write>(expr: Expression, output: Rc<RefCell<W>>, scope: Rc<Scope>) -> Expression {
    match expr {
        Expression::FuncCall(func_call) => {
            if func_call.ident == "print" {
                let value = eval_expr(*func_call.arg, Rc::clone(&output), Rc::clone(&scope));
                let result = expr_to_str(value, Rc::clone(&output), scope);
                write!(output.borrow_mut(), "{}", &result).unwrap();
                Expression::Null
            } else if func_call.ident == "println" {
                let value = eval_expr(*func_call.arg, Rc::clone(&output), Rc::clone(&scope));
                let result = expr_to_str(value, Rc::clone(&output), Rc::clone(&scope));
                writeln!(output.borrow_mut(), "{}", &result).unwrap();
                Expression::Null
            } else if func_call.ident == "add" {
                Expression::Int(fun_add(scope, output, *func_call.arg))
            } else if func_call.ident == "ret" || func_call.ident == "return" {
                Expression::Return(Box::new(eval_expr(
                    *func_call.arg,
                    output,
                    Rc::clone(&scope),
                )))
            } else {
                if let Some(expression) = scope.get(func_call.ident.clone()) {
                    if let Expression::Closure(closure) = expression {
                        call_func(closure.func_expr, func_call, output, closure.scope)
                    } else if let Expression::FuncExpr(func_expr) = expression {
                        call_func(func_expr, func_call, output, scope)
                    } else {
                        panic!("{:#?} is not callable", expression);
                    }
                } else {
                    panic!(
                        "Attempted to call {}, which is not defined",
                        func_call.ident
                    );
                }
            }
        }
        Expression::VarDecl(assignment) => {
            let value = eval_expr(assignment.expr, output, Rc::clone(&scope));

            scope.assign(assignment.ident, value.clone());

            value
        }
        Expression::ReAssignment(assignment) => match scope.get(assignment.ident.clone()) {
            Some(_) => scope.set(
                assignment.ident,
                eval_expr(assignment.expr, output, Rc::clone(&scope)),
            ),
            None => {
                panic!(
                    "Attempted to assign to undefined variable {}",
                    assignment.ident
                );
            }
        },
        Expression::String(string) => Expression::String(string),
        Expression::Touple(value) => Expression::Touple(value),
        Expression::Int(value) => Expression::Int(value),
        Expression::Variable(ident) => {
            if let Some(value) = scope.get(ident) {
                return value.clone();
            }
            Expression::Null
        }
        Expression::FuncExpr(func_expr) => Expression::Closure(Box::new(Closure {
            func_expr,
            scope: Rc::clone(&scope),
        })),
        _ => {
            panic!("Failed to eval expression {:?}", expr);
        }
    }
}

pub fn expr_to_str<W: Write>(expr: Expression, output: Rc<RefCell<W>>, scope: Rc<Scope>) -> String {
    match expr {
        Expression::String(string) => string,
        Expression::Int(int) => int.to_string(),
        Expression::Touple(value) => {
            let mut result = String::new();
            for expr in value {
                let item = eval_expr(expr, Rc::clone(&output), Rc::clone(&scope));
                result.push_str(&expr_to_str(item, Rc::clone(&output), Rc::clone(&scope)));
            }
            result
        }
        Expression::Null => String::from("null"),
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
) -> Expression {
    let child_scope = RefCell::new(Rc::new(Scope::create(scope)));
    match *func_call.arg {
        Expression::Touple(args) => {
            for (i, arg) in args.iter().enumerate() {
                let ident = func_expr.args[i].clone();
                child_scope.borrow().assign(
                    ident,
                    eval_expr(
                        arg.clone(),
                        Rc::clone(&output),
                        Rc::clone(&child_scope.borrow()),
                    ),
                );
            }
        }
        _ => {
            if func_expr.args.len() >= 1 {
                let ident = func_expr.args[0].clone();
                child_scope.borrow().assign(
                    ident,
                    eval_expr(
                        *func_call.arg,
                        Rc::clone(&output),
                        Rc::clone(&child_scope.borrow()),
                    ),
                );
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
                eval_expr(expr, Rc::clone(&output), scope);
            }
        }
    }
    Expression::Null
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
                    ident: String::from("println"),
                    arg: Box::new(Expression::Variable(String::from("x"))),
                }),
            ],
        };

        let output = Rc::new(RefCell::new(Vec::new()));

        eval_program(program, Rc::clone(&output));
        let output = output.borrow().clone();

        assert_eq!(String::from_utf8(output).unwrap(), "2\n");
    }
}
