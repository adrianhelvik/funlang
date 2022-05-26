use crate::types::*;

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

pub fn eval_expr(expr: Expression, output: &mut String) -> Expression {
    match expr {
        Expression::FuncCall(func_call) => {
            if func_call.ident == "print" {
                let mut inner_output = String::new();
                let value = eval_expr(*func_call.arg, &mut inner_output);
                let result = expr_to_str(value, &mut inner_output);
                output.push_str(&inner_output);
                output.push_str(&result);
                Expression::Null
            } else if func_call.ident == "println" {
                let mut inner_output = String::new();
                let value = eval_expr(*func_call.arg, &mut inner_output);
                let result = expr_to_str(value, &mut inner_output);
                output.push_str(&inner_output);
                output.push_str(&result);
                output.push('\n');
                Expression::Null
            } else if func_call.ident == "add" {
                Expression::Int(fun_add(*func_call.arg))
            } else {
                panic!("Call to undefined function {}", func_call.ident);
            }
        }
        Expression::Assignment(assignment) => {
            let value = eval_expr(assignment.expr, output);

            value
        }
        Expression::String(string) => Expression::String(string),
        Expression::Touple(value) => Expression::Touple(value),
        Expression::Int(value) => Expression::Int(value),
        Expression::Variable(ident) => Expression::Null,
        _ => {
            panic!("Failed to eval expression {:?}", expr);
        }
    }
}

pub fn eval_program(program: Program) -> String {
    let mut output = String::new();

    for expr in program.expressions {
        eval_expr(expr, &mut output);
    }

    return output;
}

pub fn expr_to_str(expr: Expression, output: &mut String) -> String {
    match expr {
        Expression::String(string) => string,
        Expression::Int(int) => int.to_string(),
        Expression::Touple(value) => {
            let mut result = String::new();
            for expr in value {
                let mut inner_output = String::new();
                let item = eval_expr(expr, &mut inner_output);
                output.push_str(&inner_output);
                result.push_str(&expr_to_str(item, output));
            }
            result
        }
        _ => {
            panic!("Failed to stringify expression {:#?}", expr);
        }
    }
}
