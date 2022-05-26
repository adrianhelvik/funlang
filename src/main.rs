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
            if c == ' ' {
                if !empty {
                    tokens.push(word.clone());
                    word = String::new();
                }
                empty = true;
            } else {
                empty = false;
                word.push(c);
            }
        }
    }

    if !empty {
        tokens.push(word.clone());
    }

    tokens
}

pub fn parse(tokens: Vec<String>) -> Program {
    let mut program = Program {
        expressions: vec![],
    };

    if let Some(expr) = parse_next_expr(tokens) {
        program.expressions.push(expr);
    }

    program.clone()
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

pub fn parse_func_call(tokens: Vec<String>) -> Option<FuncCall> {
    if tokens.len() < 2 {
        None
    } else {
        let remaining = (&tokens[1..]).to_vec();
        if let Some(expr) = parse_next_expr(remaining) {
            Some(FuncCall {
                ident: tokens[0].clone(),
                args: vec![expr],
            })
        } else {
            panic!("Missing argument for function call");
        }
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

pub fn parse_next_expr(tokens: Vec<String>) -> Option<Expression> {
    if tokens.len() == 0 {
        None
    } else if let Some(value) = parse_str(tokens[0].clone()) {
        Some(Expression::String(value))
    } else if let Some(value) = parse_int(tokens[0].clone()) {
        Some(Expression::Int(value))
    } else if let Some(value) = parse_func_call(tokens.clone()) {
        Some(Expression::FuncCall(value))
    } else {
        panic!("Failed to parse expression from tokens: {:?}", tokens);
    }
}

pub fn main() {
    println!("Hello, world!");
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    ident: String,
    args: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    String(String),
    Int(i64),
    FuncCall(FuncCall),
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

    /*
    #[test]
    pub fn it_can_parse_a_nonary_func_call() {
        let ast = parse_func_call(vec![String::from("foo")]);

        assert_eq!(
            ast,
            FuncCall {
                ident: String::from("foo")
            }
        )
    }
    */

    #[test]
    pub fn it_can_parse_a_string_expression() {
        let expression = parse_next_expr(vec![String::from("\"Hello world\"")]).unwrap();

        assert_eq!(expression, Expression::String(String::from("Hello world")));
    }

    #[test]
    pub fn it_can_parse_an_int_expression() {
        let expression = parse_next_expr(vec![String::from("123")]).unwrap();

        assert_eq!(expression, Expression::Int(123));
    }

    #[test]
    pub fn it_can_parse_a_unary_func_call() {
        let expression = parse_next_expr(vec![String::from("foo"), String::from("123")]).unwrap();

        assert_eq!(
            expression,
            Expression::FuncCall(FuncCall {
                ident: String::from("foo"),
                args: vec![Expression::Int(123),]
            })
        );
    }

    #[test]
    pub fn it_can_parse_a_program_with_a_func_call() {
        let ast = parse(lex("print \"Hello world\""));

        assert_eq!(
            ast,
            Program {
                expressions: vec![Expression::FuncCall(FuncCall {
                    ident: String::from("print"),
                    args: vec![Expression::String(String::from("Hello world"))]
                }),],
            }
        );
    }

    /*
    #[test]
    pub fn it_can_parse_a_program_with_multiple_func_calls() {
        let ast = parse(lex("print \"Hello world\" print \"Foo bar\""));

        assert_eq!(
            ast,
            Program {
                expressions: vec![
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        args: vec![Expression::String(String::from("Hello world"))]
                    }),
                    Expression::FuncCall(FuncCall {
                        ident: String::from("print"),
                        args: vec![Expression::String(String::from("Foo bar"))]
                    }),
                ],
            }
        );
    }
    */
}
