use crate::types::{Loc, Token};
use std::cell::RefCell;

pub fn lex(source: &str) -> Vec<Token> {
    let tokens = RefCell::new(Vec::new());
    let word = RefCell::new(String::new());
    let mut string = false;

    let line = RefCell::new(1 as usize);
    let column = RefCell::new(1 as usize);

    let insert = || {
        if word.borrow().len() == 0 {
            return;
        }
        {
            tokens.borrow_mut().push(Token::new(
                word.borrow().clone(),
                Loc::new(line.borrow().clone(), column.borrow().clone()),
            ));
        }
        if *word.borrow() == "\n" {
            *line.borrow_mut() += 1;
            column.replace(1);
        } else {
            *column.borrow_mut() += word.borrow().len();
        }
        word.replace(String::new());
    };

    let add_to_word = |value: char| {
        word.borrow_mut().push(value);
    };

    let mut in_dot = false;

    for (_i, c) in source.chars().enumerate() {
        if in_dot {
            if c == '.' {
                add_to_word(c);
                insert();
                continue;
            } else {
                insert();
            }
            in_dot = false;
        }

        if c == '"' {
            if string {
                add_to_word(c);
                insert();
            } else {
                insert();
                add_to_word(c);
            }
            string = !string;
        } else if string {
            add_to_word(c);
        } else {
            match c {
                '.' => {
                    insert();
                    in_dot = true;
                    add_to_word(c);
                },
                ' ' => {
                    insert();
                    *column.borrow_mut() += 1;
                }
                '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '=' => {
                    insert();
                    add_to_word(c);
                    insert();
                }
                _ => {
                    add_to_word(c);
                }
            }
        }
    }

    insert();

    tokens.take()
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::types::{Loc, Token};
    use pretty_assertions_sorted::assert_eq;

    fn lex_str(source: &str) -> Vec<String> {
        lex(source)
            .iter()
            .map(|token| token.value.clone())
            .collect()
    }

    #[test]
    pub fn it_can_tokenize_a_word() {
        let tokens = lex_str("abc");

        assert_eq!(tokens, vec![String::from("abc")],);
    }

    #[test]
    pub fn it_can_split_words() {
        let tokens = lex_str("abc def geh");

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
        let tokens = lex_str("print \"Hello world!\"");

        assert_eq!(
            tokens,
            vec![String::from("print"), String::from("\"Hello world!\""),],
        );
    }

    #[test]
    pub fn it_can_parse_parens() {
        let tokens = lex_str("(){}[]");

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
        let tokens = lex_str("print(\"Hello\", \"world\")");

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

    #[test]
    pub fn it_stores_the_source_loc_with_tokens() {
        let tokens = lex("print(\"Hello\", \"world\")");

        assert_eq!(
            tokens,
            vec![
                Token {
                    value: String::from("print"),
                    loc: Loc { line: 1, column: 1 },
                },
                Token {
                    value: String::from("("),
                    loc: Loc { line: 1, column: 6 },
                },
                Token {
                    value: String::from("\"Hello\""),
                    loc: Loc { line: 1, column: 7 },
                },
                Token {
                    value: String::from(","),
                    loc: Loc {
                        line: 1,
                        column: 14
                    },
                },
                Token {
                    value: String::from("\"world\""),
                    loc: Loc {
                        line: 1,
                        column: 16
                    },
                },
                Token {
                    value: String::from(")"),
                    loc: Loc {
                        line: 1,
                        column: 23
                    },
                },
            ]
        );
    }

    #[test]
    pub fn it_stores_the_source_loc_with_tokens_simpler() {
        let tokens = lex("(\"Hello\")");

        assert_eq!(
            tokens,
            vec![
                Token {
                    value: String::from("("),
                    loc: Loc { line: 1, column: 1 },
                },
                Token {
                    value: String::from("\"Hello\""),
                    loc: Loc { line: 1, column: 2 },
                },
                Token {
                    value: String::from(")"),
                    loc: Loc { line: 1, column: 9 },
                },
            ]
        );
    }

    #[test]
    pub fn it_includes_newline_tokens() {
        let tokens = lex("print(\"Hello\")\nprint(\"World\")");

        assert_eq!(
            tokens,
            vec![
                Token {
                    value: String::from("print"),
                    loc: Loc { line: 1, column: 1 },
                },
                Token {
                    value: String::from("("),
                    loc: Loc { line: 1, column: 6 },
                },
                Token {
                    value: String::from("\"Hello\""),
                    loc: Loc { line: 1, column: 7 },
                },
                Token {
                    value: String::from(")"),
                    loc: Loc {
                        line: 1,
                        column: 14
                    },
                },
                Token {
                    value: String::from("\n"),
                    loc: Loc {
                        line: 1,
                        column: 15
                    },
                },
                Token {
                    value: String::from("print"),
                    loc: Loc { line: 2, column: 1 },
                },
                Token {
                    value: String::from("("),
                    loc: Loc { line: 2, column: 6 },
                },
                Token {
                    value: String::from("\"World\""),
                    loc: Loc { line: 2, column: 7 },
                },
                Token {
                    value: String::from(")"),
                    loc: Loc {
                        line: 2,
                        column: 14
                    },
                },
            ]
        );
    }

    #[test]
    pub fn it_can_lex_for_expressions() {
        let tokens = lex("for i in 0..10 {}");

        let token_parts: Vec<&str> = tokens
            .iter()
            .map(|token| token.value.as_ref())
            .collect();

        assert_eq!(
            vec!["for", "i", "in", "0", "..", "10", "{", "}"],
            token_parts,
        );
    }
}
