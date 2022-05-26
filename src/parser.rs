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
    } else if let Some((value, tokens)) = parse_func_call(tokens.clone()) {
        Some((Expression::FuncCall(value), tokens))
    } else {
        None
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub ident: String,
    pub arg: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    String(String),
    Int(i64),
    FuncCall(FuncCall),
    Touple(Vec<Expression>),
    Null,
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
