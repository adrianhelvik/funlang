use std::env;
use std::fs;

pub fn lex(source: &str) -> Vec<String> {
    let mut tokens = vec![];
    let mut word = String::new();
    let mut empty = true;
    let mut string = false;

    for (_i, c) in source.chars().enumerate() {
        if c == '"' {
            if string {
                word.push(c);
                tokens.push(word.clone());
                word = String::new();
                empty = true;
            } else {
                if !empty {
                    tokens.push(word.clone());
                }
                word = String::new();
                word.push(c);
                empty = false;
            }
            string = !string;
        } else if string {
            word.push(c);
        } else {
            match c {
                ' ' | '\n' => {
                    if !empty {
                        tokens.push(word.clone());
                        word = String::new();
                    }
                    empty = true;
                }
                '(' | ')' | '{' | '}' | '[' | ']' | ',' => {
                    if !empty {
                        tokens.push(word.clone());
                        word = String::new();
                    }
                    tokens.push(c.to_string());
                    empty = true;
                }
                _ => {
                    empty = false;
                    word.push(c);
                }
            }
        }
    }

    if !empty {
        tokens.push(word.clone());
    }

    tokens
}

pub fn parse(input: Vec<String>) -> Program {
    let mut program = Program {
        expressions: vec![],
    };

    let mut tokens = input.clone();

    let mut remaining_tokens = tokens.len();

    while let Some((expr, next_tokens)) = parse_next_expr(tokens) {
        program.expressions.push(expr);
        tokens = next_tokens;

        if tokens.len() == remaining_tokens {
            panic!("Infinite loop in parse()");
        }

        remaining_tokens = tokens.len();
    }

    if remaining_tokens > 0 {
        panic!("Faild to parse {:#?}", &input[remaining_tokens..]);
    }

    program.clone()
}

pub fn take(token: &str, tokens: Vec<String>) -> Option<Vec<String>> {
    if tokens.len() > 0 && tokens[0] == token {
        Some((&tokens[1..]).to_vec())
    } else {
        None
    }
}

pub fn parse_touple(tokens: Vec<String>) -> Option<(Expression, Vec<String>)> {
    if let Some(next_tokens) = take("(", tokens) {
        let mut tokens = next_tokens.clone();
        let mut expressions = Vec::<Expression>::new();

        while let Some((expr, next_tokens)) = parse_next_expr(tokens.clone()) {
            tokens = next_tokens.clone();
            expressions.push(expr);

            if let Some(next_tokens) = take(",", tokens.clone()) {
                tokens = next_tokens;
            }
        }

        if let Some(tokens) = take(")", tokens.clone()) {
            Some((Expression::Touple(expressions), tokens.clone()))
        } else {
            None
        }
    } else {
        None
    }
}

pub fn lax(source: &str) -> Program {
    parse(lex(source))
}

pub fn parse_str(token: String) -> Option<String> {
    let mut value = String::new();

    for (i, c) in token.chars().enumerate() {
        if i == 0 && c != '"' {
            return None;
        }
        if i > 0 {
            value.push(c);
        }
    }

    value.pop();

    Some(value)
}

pub fn take_ident(tokens: Vec<String>) -> Option<(String, Vec<String>)> {
    if tokens.len() == 0 {
        None
    } else {
        match tokens[0].as_str() {
            "(" | ")" | "{" | "}" | "[" | "]" | "," => None,
            _ => Some((tokens[0].clone(), (&tokens[1..]).to_vec())),
        }
    }
}

pub fn parse_func_call(tokens: Vec<String>) -> Option<(FuncCall, Vec<String>)> {
    if let Some((ident, tokens)) = take_ident(tokens) {
        if let Some((expr, tokens)) = parse_next_expr(tokens) {
            Some((
                FuncCall {
                    ident,
                    arg: Box::new(expr),
                },
                tokens,
            ))
        } else {
            None
        }
    } else {
        None
    }
}

fn digit_to_int(ch: char) -> Option<i64> {
    match ch {
        '0' => Some(0),
        '1' => Some(1),
        '2' => Some(2),
        '3' => Some(3),
        '4' => Some(4),
        '5' => Some(5),
        '6' => Some(6),
        '7' => Some(7),
        '8' => Some(8),
        '9' => Some(9),
        _ => None,
    }
}

pub fn parse_int(token: String) -> Option<i64> {
    let mut sum = 0;
    let len = token.len();

    for (i, c) in token.chars().enumerate() {
        let exponent = u32::try_from(len - i - 1).unwrap();
        if let Some(digit) = digit_to_int(c) {
            sum += digit * (10 as i64).pow(exponent);
        } else {
            return None;
        }
    }

    Some(sum)
}

pub fn parse_next_expr(tokens: Vec<String>) -> Option<(Expression, Vec<String>)> {
    if tokens.len() == 0 {
        None
    } else if let Some(value) = parse_str(tokens[0].clone()) {
        Some((Expression::String(value), (&tokens[1..]).to_vec()))
    } else if let Some(value) = parse_int(tokens[0].clone()) {
        Some((Expression::Int(value), (&tokens[1..]).to_vec()))
    } else if let Some((value, tokens)) = parse_touple(tokens.clone()) {
        Some((value, tokens))
    } else if let Some((value, tokens)) = parse_func_call(tokens.clone()) {
        Some((Expression::FuncCall(value), tokens))
    } else {
        None
    }
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
        Expression::String(string) => Expression::String(string),
        Expression::Touple(value) => Expression::Touple(value),
        _ => {
            panic!("Failed to eval expression {:#?}", expr);
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

pub fn eval(source: &str) -> String {
    eval_program(parse(lex(source)))
}

pub fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        panic!("You must supply a file type");
    }
    let filename = args[2].clone();

    let contents = fs::read_to_string(filename).expect("Failed to read file");

    println!("{}", eval(&contents));
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    ident: String,
    arg: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    String(String),
    Int(i64),
    FuncCall(FuncCall),
    Touple(Vec<Expression>),
    Null,
}

#[cfg(test)]
pub mod tests {
    use super::*;

    /* Lex */

    #[test]
    pub fn it_can_tokenize_a_word() {
        let tokens = lex("abc");

        assert_eq!(tokens, vec![String::from("abc")],);
    }

    #[test]
    pub fn it_can_split_words() {
        let tokens = lex("abc def geh");

        assert_eq!(
            tokens,
            vec![
                String::from("abc"),
                String::from("def"),
                String::from("geh")
            ],
        );
    }

    #[test]
    pub fn it_can_parse_strings() {
        let tokens = lex("print \"Hello world!\"");

        assert_eq!(
            tokens,
            vec![String::from("print"), String::from("\"Hello world!\""),],
        );
    }

    #[test]
    pub fn it_can_parse_parens() {
        let tokens = lex("(){}[]");

        assert_eq!(
            tokens,
            vec![
                String::from("("),
                String::from(")"),
                String::from("{"),
                String::from("}"),
                String::from("["),
                String::from("]"),
            ]
        );
    }

    #[test]
    pub fn it_can_lex_a_binary_func_call() {
        let tokens = lex("print(\"Hello\", \"world\")");

        assert_eq!(
            tokens,
            vec![
                String::from("print"),
                String::from("("),
                String::from("\"Hello\""),
                String::from(","),
                String::from("\"world\""),
                String::from(")"),
            ]
        );
    }

    /* Parse */

    #[test]
    pub fn it_can_construct_an_ast() {
        let ast = Program {
            expressions: vec![],
        };

        assert_eq!(
            ast,
            Program {
                expressions: vec![]
            }
        );
    }

    #[test]
    pub fn it_can_parse_an_empty_program() {
        let ast = lax("");

        assert_eq!(
            ast,
            Program {
                expressions: vec![]
            }
        );
    }

    #[test]
    pub fn it_can_parse_a_string() {
        let ast = parse_str(String::from("\"Hello world\"")).unwrap();

        assert_eq!(ast, String::from("Hello world"));
    }

    #[test]
    pub fn it_can_parse_a_string_expression() {
        let (expression, _tokens) = parse_next_expr(vec![String::from("\"Hello world\"")]).unwrap();

        assert_eq!(expression, Expression::String(String::from("Hello world")));
    }

    #[test]
    pub fn it_can_parse_an_int_expression() {
        let (expression, _tokens) = parse_next_expr(vec![String::from("123")]).unwrap();

        assert_eq!(expression, Expression::Int(123));
    }

    #[test]
    pub fn it_can_parse_a_unary_func_call() {
        let (expression, tokens) =
            parse_next_expr(vec![String::from("foo"), String::from("123")]).unwrap();

        assert_eq!(
            expression,
            Expression::FuncCall(FuncCall {
                ident: String::from("foo"),
                arg: Box::new(Expression::Int(123)),
            })
        );

        assert_eq!(tokens, Vec::<String>::new());
    }

    #[test]
    pub fn it_can_parse_a_program_with_a_func_call() {
        let ast = parse(lex("print \"Hello world\""));

        assert_eq!(
            ast,
            Program {
                expressions: vec![Expression::FuncCall(FuncCall {
                    ident: String::from("print"),
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                }),],
            }
        );
    }

    #[test]
    pub fn it_can_parse_a_program_with_multiple_func_calls() {
        let ast = parse(lex("print \"Hello world\" print \"Foo bar\""));

        assert_eq!(
            ast,
            Program {
                expressions: vec![
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        arg: Box::new(Expression::String(String::from("Hello world"))),
                    }),
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        arg: Box::new(Expression::String(String::from("Foo bar"))),
                    }),
                ],
            }
        );
    }

    #[test]
    pub fn it_can_parse_a_binary_func_call() {
        let (func_call, _tokens) = parse_func_call(lex("print(\"Hello\", \"world\")")).unwrap();

        assert_eq!(
            func_call,
            FuncCall {
                ident: String::from("print"),
                arg: Box::new(Expression::Touple(vec![
                    Expression::String(String::from("Hello")),
                    Expression::String(String::from("world")),
                ])),
            }
        );
    }

    #[test]
    pub fn it_can_parse_a_touple() {
        let (touple, _tokens) = parse_touple(lex("()")).unwrap();

        assert_eq!(touple, Expression::Touple(Vec::<Expression>::new()));
    }

    #[test]
    pub fn it_can_parse_a_touple_with_expressions() {
        let (touple, _tokens) = parse_touple(lex("(\"foo\" \"bar\")")).unwrap();

        assert_eq!(
            touple,
            Expression::Touple(vec![
                Expression::String(String::from("foo")),
                Expression::String(String::from("bar")),
            ])
        );
    }

    /* Eval */

    #[test]
    pub fn it_can_eval_a_func_call() {
        let output = eval_program(parse(lex("print \"Hello world\"")));

        assert_eq!(output, "Hello world");
    }

    #[test]
    pub fn it_can_eval_addition() {
        let output = eval_program(parse(lex("print add(105, 20)")));

        assert_eq!(output, "125");
    }
}
