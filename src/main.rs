pub mod interpreter;
pub mod lexer;
pub mod parser;

use lexer::lex;
use parser::parse;
use parser::Program;
use std::env;
use std::fs;

pub fn eval(source: &str) -> String {
    interpreter::eval_program(parse(lex(source)))
}

pub fn lax(source: &str) -> Program {
    parse(lex(source))
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

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::parser::*;

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
        let output = interpreter::eval_program(parse(lex("print \"Hello world\"")));

        assert_eq!(output, "Hello world");
    }

    #[test]
    pub fn it_can_eval_addition() {
        let output = interpreter::eval_program(parse(lex("print add(105, 20)")));

        assert_eq!(output, "125");
    }
}
