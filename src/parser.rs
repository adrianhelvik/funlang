use crate::types::*;

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
        panic!("Failed to parse {:#?}", &input[remaining_tokens..]);
    }

    program.clone()
}

pub fn parse_assignment(tokens: Vec<String>) -> Option<(Assignment, Vec<String>)> {
    if let Some((ident, tokens)) = take_ident(tokens) {
        if let Some(tokens) = take(":", tokens) {
            if let Some((expr, tokens)) = parse_next_expr(tokens) {
                Some((Assignment { ident, expr }, tokens))
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
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

fn take_ident(tokens: Vec<String>) -> Option<(String, Vec<String>)> {
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
    } else if let Some((value, tokens)) = parse_assignment(tokens.clone()) {
        Some((Expression::Assignment(Box::new(value)), tokens))
    } else if let Some((value, tokens)) = parse_func_call(tokens.clone()) {
        Some((Expression::FuncCall(value), tokens))
    } else if let Some((value, tokens)) = take_ident(tokens.clone()) {
        Some((Expression::Variable(value), tokens))
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

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::lexer::lex;

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
        let ast = parse(lex(""));

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

    #[test]
    pub fn it_can_parse_assignment() {
        let (assignment, _tokens) = parse_assignment(lex("x: 10")).unwrap();

        assert_eq!(
            assignment,
            Assignment {
                ident: String::from("x"),
                expr: Expression::Int(10),
            }
        );
    }

    #[test]
    pub fn it_can_parse_variables() {
        let (func_call, _tokens) = parse_func_call(lex("print x")).unwrap();

        println!("{:#?}", func_call);

        assert_eq!(
            func_call,
            FuncCall {
                ident: String::from("print"),
                arg: Box::new(Expression::Variable(String::from("x"))),
            }
        )
    }

    /*
    #[test]
    pub fn you_can_assign_variables() {
        let source = r#"
            x: 10
            print x
        "#;

        assert_eq!(parse(lex(source)), Program {
            expressions: vec![
                Expression::Assignment(Assignment {
                    ident: "x",
                    expr: Expression::Int(10),
                })
        Expression::FuncCall(FuncCall {
            ident: "print",
            expr: Expression::Variable
        })
            ],
        })
    }
    */
}
