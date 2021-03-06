use crate::types::*;
use std::collections::HashMap;

pub fn eval_program(program: Program) -> String {
    let mut output = String::new();
    let mut scope = HashMap::<String, Expression>::new();

    for expr in program.expressions {
        eval_expr(expr, &mut output, &mut scope, 0);
    }

    return output;
}

pub fn fun_add(expr: Expression) -> i64 {
    match expr {
        Expression::Touple(expressions) => {
            let mut result = 0;
            for expr in expressions {
                result += fun_add(expr);
            }
            result
        }
        Expression::Int(int) => int,
        _ => {
            panic!("Failed to add {:#?}", expr);
        }
    }
}

fn lookup(
    scope: &HashMap<String, Expression>,
    ident: &str,
    scope_level: i32,
) -> Option<Expression> {
    for i in 0..(scope_level + 1) {
        let id = format!("{}{}", scope_level - i, ident);
        if let Some(expression) = scope.get(&id) {
            return Some(expression.clone());
        }
    }
    None
}

pub fn eval_expr(
    expr: Expression,
    output: &mut String,
    scope: &mut HashMap<String, Expression>,
    scope_level: i32,
) -> Expression {
    match expr {
        Expression::FuncCall(func_call) => {
            if func_call.ident == "print" {
                let mut inner_output = String::new();
                let value = eval_expr(*func_call.arg, &mut inner_output, scope, scope_level);
                let result = expr_to_str(value, &mut inner_output, scope, scope_level);
                output.push_str(&inner_output);
                output.push_str(&result);
                Expression::Null
            } else if func_call.ident == "println" {
                let mut inner_output = String::new();
                let value = eval_expr(*func_call.arg, &mut inner_output, scope, scope_level);
                let result = expr_to_str(value, &mut inner_output, scope, scope_level);
                output.push_str(&inner_output);
                output.push_str(&result);
                output.push('\n');
                Expression::Null
            } else if func_call.ident == "add" {
                Expression::Int(fun_add(*func_call.arg))
            } else {
                if let Some(expression) = lookup(&scope, &func_call.ident, scope_level) {
                    if let Expression::Block(block) = expression {
                        for expr in block.expressions.clone() {
                            eval_expr(expr, output, scope, scope_level + 1);
                        }
                        Expression::Null
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
        Expression::Assignment(assignment) => {
            let value = eval_expr(assignment.expr, output, scope, scope_level);
            let id = format!("{}{}", scope_level, assignment.ident);

            scope.insert(id, value.clone());

            value
        }
        Expression::ReAssignment(assignment) => {
            let value = eval_expr(assignment.expr, output, scope, scope_level);
            for i in 0..(scope_level + 1) {
                let id = format!("{}{}", scope_level - i, assignment.ident);
                if scope.contains_key(&id) {
                    scope.insert(id, value.clone());
                    return value;
                }
            }
            panic!(
                "Attempted to assign to undefined variable {}",
                assignment.ident
            );
        }
        Expression::String(string) => Expression::String(string),
        Expression::Touple(value) => Expression::Touple(value),
        Expression::Int(value) => Expression::Int(value),
        Expression::Variable(ident) => {
            if let Some(value) = lookup(&scope, &ident, scope_level) {
                return value.clone();
            }
            Expression::Null
        }
        Expression::Block(block) => Expression::Block(block),
        _ => {
            panic!("Failed to eval expression {:?}", expr);
        }
    }
}

pub fn expr_to_str(
    expr: Expression,
    output: &mut String,
    scope: &mut HashMap<String, Expression>,
    scope_level: i32,
) -> String {
    match expr {
        Expression::String(string) => string,
        Expression::Int(int) => int.to_string(),
        Expression::Touple(value) => {
            let mut result = String::new();
            for expr in value {
                let mut inner_output = String::new();
                let item = eval_expr(expr, &mut inner_output, scope, scope_level);
                output.push_str(&inner_output);
                result.push_str(&expr_to_str(item, output, scope, scope_level));
            }
            result
        }
        Expression::Null => String::from("null"),
        _ => {
            panic!("Failed to stringify expression {:#?}", expr);
        }
    }
}
