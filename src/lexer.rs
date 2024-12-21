use crate::types::{Loc, Token};
use std::cell::RefCell;

struct Lexer<'a> {
    source: &'a str,
    tokens: RefCell<Vec<Token>>,
    word: String,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Lexer {
            source,
            tokens: RefCell::new(vec![]),
            word: String::new(),
            line: 1,
            column: 1,
        }
    }

    fn insert(&mut self) {
        if self.word.len() == 0 {
            return;
        }
        let unescaped = unescape::unescape(&self.word).expect("Malformed string");
        {
            self.tokens.borrow_mut().push(Token::new(
                &unescaped,
                Loc::new(self.line.clone(), self.column.clone()),
            ));
        }
        if self.word == "\n" {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += self.word.len();
        }
        self.word = String::new();
    }

    fn add_to_word(&mut self, value: char) {
        self.word.push(value);
    }

    fn lex(&mut self) -> Vec<Token> {
        let mut string = false;
        let mut in_dot = false;
        let mut escaped = false;

        for (_i, c) in self.source.chars().enumerate() {
            if in_dot {
                if c == '.' {
                    self.add_to_word(c);
                    self.insert();
                    continue;
                } else {
                    self.insert();
                }
                in_dot = false;
            }

            if c == '"' && !escaped {
                if string {
                    self.add_to_word(c);
                    self.insert();
                } else {
                    self.insert();
                    self.add_to_word(c);
                }
                string = !string;
            } else if string {
                if c == '\n' {
                    self.line += 1;
                    self.column = 1;
                }

                if escaped {
                    // We only handle escaped " here,
                    // so add back the backslash for
                    // all other characters.
                    if c != '\"' {
                        self.add_to_word('\\')
                    }
                    self.add_to_word(c);
                    escaped = false;
                } else if c == '\\' {
                    escaped = true;
                } else {
                    self.add_to_word(c);
                }
            } else {
                match c {
                    '.' => {
                        self.insert();
                        in_dot = true;
                        self.add_to_word(c);
                    }
                    ' ' => {
                        self.insert();
                        self.column += 1;
                    }
                    '\n' | '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '=' => {
                        self.insert();
                        self.add_to_word(c);
                        self.insert();
                    }
                    _ => {
                        self.add_to_word(c);
                    }
                }
            }
        }

        self.insert();

        self.tokens.take()
    }
}

pub fn lex(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source);

    lexer.lex()
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

        assert_eq!(tokens, vec!["abc"],);
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

        let token_parts: Vec<&str> = tokens.iter().map(|token| token.value.as_ref()).collect();

        assert_eq!(
            vec!["for", "i", "in", "0", "..", "10", "{", "}"],
            token_parts,
        );
    }
}
