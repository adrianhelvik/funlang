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
