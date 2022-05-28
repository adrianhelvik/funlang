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
                '(' | ')' | '{' | '}' | '[' | ']' | ',' | ':' | '=' => {
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

#[cfg(test)]
pub mod tests {
    use super::*;

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
}
