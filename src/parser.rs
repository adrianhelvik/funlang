use crate::types::Possibly::{None, Some};
use crate::types::*;

pub fn parse(input: Tokens) -> Result<Program, ParseError> {
    let (expressions, tokens) = parse_expr_list(input);

    let tokens = skip("\n", tokens);

    if tokens.len() > 0 {
        return Err(ParseError {
            message: format!("Unexpected token '{}'", tokens[0].value),
            token: tokens[0].clone(),
        });
    }

    Ok(Program { expressions })
}

fn parse_expr_list(tokens: Tokens) -> (Vec<Expression>, Tokens) {
    let tokens = skip("\n", tokens);

    let mut expressions = vec![];
    let mut tokens = tokens.clone();
    let mut remaining_tokens = tokens.len();

    while let Some((expr, next_tokens)) = parse_next_expr_allow_newline(tokens.clone()) {
        expressions.push(expr);
        tokens = next_tokens;

        if tokens.len() == remaining_tokens {
            panic!("Infinite loop in parse()");
        }

        remaining_tokens = tokens.len();
    }

    (expressions, tokens.clone())
}

fn parse_var_decl(tokens: Tokens) -> Possibly<(VarDecl, Tokens), ParseError> {
    if let Some(tokens) = take("let", tokens) {
        if let Some((ident, tokens)) = take_ident(tokens) {
            if let Some(tokens) = take("=", tokens) {
                if let Some((expr, tokens)) = parse_next_expr(tokens) {
                    Some((VarDecl { ident, expr }, tokens))
                } else {
                    None
                }
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

fn parse_reassignment(tokens: Tokens) -> Possibly<(ReAssignment, Tokens), ParseError> {
    if let Some((ident, tokens)) = take_ident(tokens) {
        if let Some(tokens) = take("=", tokens) {
            if let Some((expr, tokens)) = parse_next_expr(tokens) {
                Some((ReAssignment { ident, expr }, tokens))
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

pub fn take(token: &str, tokens: Tokens) -> Possibly<Tokens, ParseError> {
    if tokens.len() > 0 && tokens[0].value == token {
        Some((&tokens[1..]).to_vec())
    } else {
        None
    }
}

pub fn parse_touple(tokens: Tokens) -> Possibly<(Expression, Tokens), ParseError> {
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

pub fn parse_str(token: String) -> Possibly<String, ParseError> {
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

fn take_ident(tokens: Tokens) -> Possibly<(String, Tokens), ParseError> {
    if tokens.len() == 0 {
        None
    } else {
        match tokens[0].value.as_str() {
            "\n" | "(" | ")" | "{" | "}" | "[" | "]" | "," => None,
            _ => Some((tokens[0].value.clone(), (&tokens[1..]).to_vec())),
        }
    }
}

pub fn parse_func_call(tokens: Tokens) -> Possibly<(FuncCall, Tokens), ParseError> {
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

pub fn parse_int(token: String) -> Possibly<i64, ParseError> {
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

pub fn parse_next_expr_allow_newline(tokens: Tokens) -> Possibly<(Expression, Tokens), ParseError> {
    let tokens = skip("\n", tokens);
    parse_next_expr(tokens)
}

pub fn parse_next_expr(tokens: Tokens) -> Possibly<(Expression, Tokens), ParseError> {
    if tokens.len() == 0 {
        None
    } else if let Some((value, tokens)) = parse_var_decl(tokens.clone()) {
        Some((Expression::VarDecl(Box::new(value)), tokens))
    } else if let Some((value, tokens)) = parse_reassignment(tokens.clone()) {
        Some((Expression::ReAssignment(Box::new(value)), tokens))
    } else if let Some(value) = parse_str(tokens[0].clone().value) {
        Some((Expression::String(value), (&tokens[1..]).to_vec()))
    } else if let Some(value) = parse_int(tokens[0].clone().value) {
        Some((Expression::Int(value), (&tokens[1..]).to_vec()))
    } else if let Some((expr, tokens)) = parse_return_expr(tokens.clone()) {
        Some((Expression::Return(Box::new(expr)), tokens))
    } else if let Some((value, tokens)) = parse_func_expr(tokens.clone()) {
        Some((Expression::FuncExpr(value), tokens))
    } else if let Some((value, tokens)) = parse_touple(tokens.clone()) {
        Some((value, tokens))
    } else if let Some((value, tokens)) = parse_func_call(tokens.clone()) {
        Some((Expression::FuncCall(value), tokens))
    } else if let Some((value, tokens)) = take_ident(tokens.clone()) {
        Some((Expression::Variable(value), tokens))
    } else {
        None
    }
}

fn parse_return_expr(tokens: Tokens) -> Possibly<(Expression, Tokens), ParseError> {
    let tokens = skip("\n", tokens);
    if let Some(tokens) = take("return", tokens) {
        let tokens = skip("\n", tokens);
        if let Some((expr, tokens)) = parse_next_expr(tokens.clone()) {
            Some((expr, tokens))
        } else {
            Some((Expression::Null, tokens))
        }
    } else {
        None
    }
}

fn digit_to_int(ch: char) -> Possibly<i64, ParseError> {
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

fn parse_func_args(tokens: Tokens) -> Possibly<(Vec<String>, Tokens), ParseError> {
    if let Some(tokens) = take("(", tokens) {
        let mut tokens = tokens.clone();
        let mut args = Vec::<String>::new();

        while let Some((ident, next_tokens)) = take_ident(tokens.clone()) {
            tokens = next_tokens.clone();
            args.push(ident);

            if let Some(next_tokens) = take(",", tokens.clone()) {
                tokens = next_tokens;
            }
        }

        if let Some(tokens) = take(")", tokens.clone()) {
            Some((args, tokens.clone()))
        } else {
            None
        }
    } else {
        None
    }
}

fn skip(token: &str, tokens: Tokens) -> Tokens {
    if tokens.len() > 0 && tokens[0].value == token {
        skip(token, (&tokens[1..]).to_vec())
    } else {
        tokens.clone()
    }
}

fn parse_func_expr(tokens: Tokens) -> Possibly<(FuncExpr, Tokens), ParseError> {
    let tokens = skip("\n", tokens.clone());

    let (args, tokens) = match parse_func_args(tokens.clone()) {
        Possibly::Some((args, tokens)) => (args, tokens),
        Possibly::None => (Vec::new(), tokens),
        Possibly::Err(e) => return Possibly::Err(e),
    };

    let tokens = skip("\n", tokens);

    if let Some(tokens) = take("{", tokens) {
        let tokens = skip("\n", tokens);
        let (expressions, tokens) = parse_expr_list(tokens);
        let tokens = skip("\n", tokens);

        if let Some(tokens) = take("}", tokens) {
            let tokens = skip("\n", tokens);
            Some((
                FuncExpr {
                    args,
                    expressions: expressions.clone(),
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

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::lexer::lex;
    use pretty_assertions_sorted::assert_eq;

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
        let ast = parse(lex("")).unwrap();

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
        let (expression, _tokens) = parse_next_expr(vec![Token::from("\"Hello world\"")]).unwrap();

        assert_eq!(expression, Expression::String(String::from("Hello world")));
    }

    #[test]
    pub fn it_can_parse_an_int_expression() {
        let (expression, _tokens) = parse_next_expr(vec![Token::from("123")]).unwrap();

        assert_eq!(expression, Expression::Int(123));
    }

    #[test]
    pub fn it_can_parse_a_unary_func_call() {
        let (expression, tokens) =
            parse_next_expr(vec![Token::from("foo"), Token::from("123")]).unwrap();

        assert_eq!(
            expression,
            Expression::FuncCall(FuncCall {
                ident: String::from("foo"),
                arg: Box::new(Expression::Int(123)),
            })
        );

        assert_eq!(tokens, Vec::<Token>::new());
    }

    #[test]
    pub fn it_can_parse_a_program_with_a_func_call() {
        let ast = parse(lex("print \"Hello world\"")).unwrap();

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
        let ast = parse(lex("print \"Hello world\" print \"Foo bar\"")).unwrap();

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
        let (assignment, _tokens) = parse_var_decl(lex("let x = 10")).unwrap();

        assert_eq!(
            assignment,
            VarDecl {
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

    #[test]
    pub fn you_can_assign_variables() {
        let source = r#"
            let x = 10
            print x
        "#;

        assert_eq!(
            parse(lex(source)).unwrap(),
            Program {
                expressions: vec![
                    Expression::VarDecl(Box::new(VarDecl {
                        ident: String::from("x"),
                        expr: Expression::Int(10),
                    })),
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        arg: Box::new(Expression::Variable(String::from("x"))),
                    })
                ],
            }
        )
    }

    #[test]
    pub fn you_can_reassign_variables() {
        let source = r#"
            let x = 1
            x = 2
            print x
        "#;

        assert_eq!(
            parse(lex(source)).unwrap(),
            Program {
                expressions: vec![
                    Expression::VarDecl(Box::new(VarDecl {
                        ident: String::from("x"),
                        expr: Expression::Int(1),
                    })),
                    Expression::ReAssignment(Box::new(ReAssignment {
                        ident: String::from("x"),
                        expr: Expression::Int(2),
                    })),
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        arg: Box::new(Expression::Variable(String::from("x"))),
                    })
                ],
            }
        )
    }

    #[test]
    pub fn you_can_create_a_func_expr() {
        let source = r#"
            {}
        "#;

        let (func_expr, tokens) = parse_func_expr(lex(source)).unwrap();

        assert_eq!(
            func_expr,
            FuncExpr {
                args: Vec::new(),
                expressions: Vec::new(),
            }
        );

        assert_eq!(tokens, Tokens::new());
    }

    #[test]
    pub fn you_can_create_a_func_expr_with_expressions() {
        let source = r#"
            {
                print "Hello world"
            }
        "#;

        let (func_expr, tokens) = parse_func_expr(lex(source)).unwrap();

        assert_eq!(
            func_expr,
            FuncExpr {
                args: Vec::new(),
                expressions: vec![Expression::FuncCall(FuncCall {
                    ident: String::from("print"),
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                })],
            }
        );

        assert_eq!(tokens, Tokens::new());
    }

    #[test]
    pub fn you_can_create_a_func_expr_with_args() {
        let source = r#"
            (x, y) { print "Hello world" }
        "#;

        let program = parse(lex(source)).unwrap();

        assert_eq!(
            program,
            Program {
                expressions: vec![Expression::FuncExpr(FuncExpr {
                    args: vec![String::from("x"), String::from("y")],
                    expressions: vec![Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        arg: Box::new(Expression::String(String::from("Hello world"))),
                    })],
                }),]
            }
        );
    }

    #[test]
    pub fn it_parses_a_sequence_of_unary_func_calls_with_an_ident_arg_correctly() {
        let source = r#"
            print a
            print b
        "#;

        let program = parse(lex(source)).unwrap();

        assert_eq!(
            program,
            Program {
                expressions: vec![
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        arg: Box::new(Expression::Variable(String::from("a"))),
                    }),
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        arg: Box::new(Expression::Variable(String::from("b"))),
                    }),
                ],
            }
        );
    }

    #[test]
    pub fn it_can_throw_a_syntax_error() {
        let source = r#"
            {
        "#;

        let result = parse(lex(source)).err().unwrap();

        assert_eq!(
            result,
            ParseError {
                message: String::from("Unexpected token '{'"),
                token: Token::new(String::from("{"), Location::new(2, 13)),
            }
        );
    }
}
