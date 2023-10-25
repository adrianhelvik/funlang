use crate::types::Possibly::{None, Some};
use crate::types::*;
use std::cell::RefCell;
use std::rc::Rc;

pub fn parse(input: Tokens) -> Result<Program, LocError> {
    inner_parse(State::new(input))
}

#[derive(Debug, PartialEq)]
struct State {
    tokens: Tokens,
    limited: bool,
    indent_level: Rc<RefCell<usize>>,
    debug: bool,
}

impl State {
    pub fn new(tokens: Tokens) -> Self {
        State {
            tokens,
            limited: false,
            indent_level: Rc::new(RefCell::new(0)),
            debug: false,
        }
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn get(&self, index: usize) -> Token {
        self.tokens.get(index).cloned().unwrap()
    }

    pub fn clone(&self) -> Self {
        State {
            tokens: self.tokens.clone(),
            limited: self.limited,
            indent_level: Rc::clone(&self.indent_level),
            debug: self.debug,
        }
    }

    pub fn iter(&self) -> std::slice::Iter<Token> {
        self.tokens.iter()
    }

    pub fn rest(&self) -> Self {
        let mut tokens = self.tokens.clone();
        tokens.remove(0);
        State {
            tokens,
            limited: self.limited,
            indent_level: Rc::clone(&self.indent_level),
            debug: self.debug,
        }
    }

    pub fn first(&self) -> Token {
        self.tokens.get(0).cloned().unwrap()
    }

    pub fn limit(&self) -> Self {
        State {
            tokens: self.tokens.clone(),
            limited: true,
            indent_level: Rc::clone(&self.indent_level.clone()),
            debug: self.debug,
        }
    }

    pub fn unlimit(&self) -> Self {
        State {
            tokens: self.tokens.clone(),
            limited: false,
            indent_level: Rc::clone(&self.indent_level.clone()),
            debug: self.debug,
        }
    }

    pub fn print(&self, message: String) {
        if !self.debug {
            return;
        }
        println!("{}{}", "  ".repeat(*self.indent_level.borrow()), message);
    }

    pub fn print_remaining(&self) {
        if !self.debug {
            return;
        }
        self.print(format!(
            "REMAINING TOKENS: {:?}",
            self.tokens
                .iter()
                .map(|t| t.value.clone())
                .collect::<Vec<String>>()
        ));
    }

    pub fn indent(&self) {
        if !self.debug {
            return;
        }
        *self.indent_level.borrow_mut() += 1;
    }

    pub fn dedent(&self) {
        if !self.debug {
            return;
        }
        let mut indent_level = self.indent_level.borrow_mut();
        if *indent_level <= 0 {
            panic!("Cannot dedent below 0");
        }
        *indent_level -= 1;
    }
}

fn inner_parse(state: State) -> Result<Program, LocError> {
    let (expressions, state) = parse_expr_list(state);

    let state = skip_all("\n", state);

    if state.len() > 0 {
        let token = state.get(0);
        return Err(LocError {
            message: format!("Unexpected token '{}'", token.value),
            loc: token.loc,
        });
    }

    Ok(Program::new(expressions))
}

fn parse_expr_list(state: State) -> (Vec<Expression>, State) {
    let state = skip_all("\n", state);

    let mut expressions = vec![];
    let mut state = state.clone();
    let mut remaining_state = state.len();

    while let Some((expr, next_state)) = parse_next_expr_allow_newline(state.clone()) {
        state.print(format!("FOUND EXPRESSION: {:?}", expr));
        state.print_remaining();

        expressions.push(expr);
        state = next_state;

        if state.len() == remaining_state {
            panic!("Infinite loop in parse()");
        }

        remaining_state = state.len();
    }

    (expressions, state.clone())
}

fn parse_var_decl(state: State) -> Possibly<(VarDecl, State), LocError> {
    if state.limited {
        return Possibly::None;
    }

    if let Option::Some((token, state)) = pick("let", &state) {
        if let Some((ident, state)) = take_ident(&state) {
            if let Some(state) = take("=", state) {
                if let Some((expr, state)) = parse_next_expr(state) {
                    Some((VarDecl { ident, expr }, state))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            Possibly::Err(LocError {
                message: "Expected identifier after let keyword".to_string(),
                loc: loc_from_first(&state, token.loc),
            })
        }
    } else {
        None
    }
}

fn loc_from_first(state: &State, fallback: Loc) -> Loc {
    if state.len() > 0 {
        state.get(0).loc.clone()
    } else {
        fallback
    }
}

fn parse_if_expr(state: State) -> Possibly<(IfExpr, State), LocError> {
    if state.limited {
        return Possibly::None;
    }

    match pick("if", &state) {
        Option::None => Possibly::None,
        Option::Some((if_kw, state)) => {
            state.print(format!("FOUND IF KEYWORD"));
            state.print_remaining();
            match parse_next_expr(state.limit()) {
                None => {
                    println!("NO CONDITION");
                    let loc = loc_from_first(&state, if_kw.loc);
                    Possibly::Err(LocError {
                        message: "Expected expression after if keyword".to_string(),
                        loc,
                    })
                }
                Some((condition, state)) => {
                    let state = state.unlimit();

                    state.print(format!("FOUND CONDITION: {:?}", condition));
                    state.print_remaining();

                    let if_expr = parse_next_expr(state.clone());
                    if if_expr == None {
                        return Possibly::Err(LocError {
                            message: "Expected expression after if condition".to_string(),
                            loc: if_kw.loc,
                        });
                    }
                    let (if_expr, state) = if_expr.unwrap();

                    let state = skip_all("\n", state);

                    let else_kw = pick("else", &state);
                    if else_kw == Option::None {
                        return Possibly::Some((
                            IfExpr {
                                condition,
                                then_expr: if_expr,
                                else_expr: Option::None,
                            },
                            state,
                        ));
                    }
                    let (else_kw, state) = else_kw.unwrap();

                    let opening_brace = pick("{", &state);
                    if opening_brace == Option::None {
                        return Possibly::Err(LocError {
                            message: "Expected opening brace after else keyword".to_string(),
                            loc: else_kw.loc,
                        });
                    }
                    let (opening_brace, state) = opening_brace.unwrap();

                    let else_expr = parse_next_expr(state.clone());
                    if else_expr == None {
                        return Possibly::Err(LocError {
                            message: "Expected expression after else keyword".to_string(),
                            loc: opening_brace.loc,
                        });
                    }
                    let (else_expr, state) = else_expr.unwrap();

                    let closing_brace = pick("}", &state);
                    if closing_brace == Option::None {
                        let loc = loc_from_first(&state, opening_brace.loc);
                        return Possibly::Err(LocError {
                            message: "Expected closing brace after else expression".to_string(),
                            loc,
                        });
                    }
                    let (_closing_brace, state) = closing_brace.unwrap();

                    Possibly::Some((
                        IfExpr {
                            condition,
                            then_expr: if_expr,
                            else_expr: Option::Some(else_expr),
                        },
                        state,
                    ))
                }
                Possibly::Err(err) => {
                    state.print(format!("CONDITION ERROR"));
                    Possibly::Err(err)
                }
            }
        }
    }
}

fn parse_reassignment(state: State) -> Possibly<(ReAssignment, State), LocError> {
    if let Some((ident, state)) = take_ident(&state) {
        if let Some(state) = take("=", state) {
            if let Some((expr, state)) = parse_next_expr(state) {
                Some((ReAssignment { ident, expr }, state))
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

fn take(expected: &str, state: State) -> Possibly<State, LocError> {
    if state.len() > 0 && state.first().value == expected {
        Some(state.rest())
    } else {
        None
    }
}

fn pick(expected: &str, state: &State) -> Option<(Token, State)> {
    if state.len() > 0 && state.first().value == expected {
        Option::Some((state.first().clone(), state.rest()))
    } else {
        Option::None
    }
}

fn parse_touple(state: State) -> Possibly<(Expression, State), LocError> {
    if let Some(next_state) = take("(", state) {
        let mut state = next_state.clone();
        let mut expressions = Vec::<Expression>::new();

        while let Some((expr, next_state)) = parse_next_expr(state.clone()) {
            state = next_state.clone();
            expressions.push(expr);

            if let Some(next_state) = take(",", state.clone()) {
                state = next_state;
            }
        }

        if let Some(state) = take(")", state.clone()) {
            Some((Expression::Touple(expressions), state.clone()))
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_str(token: String) -> Possibly<String, LocError> {
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

fn take_ident(state: &State) -> Possibly<(String, State), LocError> {
    if state.len() == 0 {
        None
    } else {
        match state.first().value.as_str() {
            "\n" | "(" | ")" | "{" | "}" | "[" | "]" | "," => None,
            _ => Some((state.first().value.clone(), state.rest())),
        }
    }
}

fn take_token(state: State) -> Possibly<(Token, State), LocError> {
    if state.len() == 0 {
        None
    } else {
        match state.first().value.as_str() {
            "\n" | "(" | ")" | "{" | "}" | "[" | "]" | "," => None,
            _ => Some((state.first().clone(), state.rest())),
        }
    }
}

fn parse_func_call(state: State) -> Possibly<(FuncCall, State), LocError> {
    if let Some((token, state)) = take_token(state) {
        if let Some((expr, state)) = parse_next_expr(state) {
            Some((
                FuncCall {
                    ident: Variable {
                        ident: token.value,
                        loc: token.loc,
                    },
                    arg: Box::new(expr),
                },
                state,
            ))
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_int(token: String) -> Possibly<i64, LocError> {
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

fn parse_next_expr_allow_newline(state: State) -> Possibly<(Expression, State), LocError> {
    let state = skip_all("\n", state);
    parse_next_expr(state)
}

fn parse_next_expr(state: State) -> Possibly<(Expression, State), LocError> {
    if state.len() == 0 {
        return None;
    };

    state.indent();

    state.print(format!(
        "parse_next_expr: {:?}",
        state
            .iter()
            .map(|t| t.value.clone())
            .collect::<Vec<String>>()
    ));

    match parse_if_expr(state.clone()) {
        Possibly::Some((if_expr, state)) => {
            state.dedent();
            return Possibly::Some((Expression::IfExpr(Box::new(if_expr)), state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        Possibly::None => {}
    }

    match parse_var_decl(state.clone()) {
        Some((value, state)) => {
            state.dedent();
            return Some((Expression::VarDecl(Box::new(value)), state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match parse_reassignment(state.clone()) {
        Some((value, state)) => {
            state.dedent();
            return Some((Expression::ReAssignment(Box::new(value)), state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match parse_str(state.first().clone().value) {
        Some(value) => {
            state.dedent();
            return Some((Expression::String(value), state.rest()));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match parse_int(state.first().clone().value) {
        Some(value) => {
            state.dedent();
            return Some((Expression::Int(value), state.rest()));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match parse_return_expr(state.clone()) {
        Some((value, state)) => {
            state.dedent();
            return Some((Expression::Return(Box::new(value)), state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match parse_func_expr(state.clone()) {
        Some((value, state)) => {
            state.dedent();
            return Some((Expression::FuncExpr(value), state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match parse_touple(state.clone()) {
        Some((value, state)) => {
            state.dedent();
            return Some((value, state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match parse_func_call(state.clone()) {
        Some((value, state)) => {
            state.dedent();
            return Some((Expression::FuncCall(value), state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    match take_token(state.clone()) {
        Some((token, state)) => {
            state.dedent();
            return Some((Expression::Variable(Variable::new(token)), state));
        }
        Possibly::Err(e) => {
            state.dedent();
            return Possibly::Err(e);
        }
        None => {}
    }

    state.dedent();

    None
}

fn parse_return_expr(state: State) -> Possibly<(Expression, State), LocError> {
    let state = skip_all("\n", state);
    if let Some(state) = take("return", state) {
        let state = skip_all("\n", state);
        if let Some((expr, state)) = parse_next_expr(state.clone()) {
            Some((expr, state))
        } else {
            Some((Expression::Null, state))
        }
    } else {
        None
    }
}

fn digit_to_int(ch: char) -> Possibly<i64, LocError> {
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

fn parse_func_args(state: State) -> Possibly<(Vec<String>, State), LocError> {
    if let Some(state) = take("(", state) {
        let mut state = state.clone();
        let mut args = Vec::<String>::new();

        while let Some((ident, next_state)) = take_ident(&state) {
            state = next_state.clone();
            args.push(ident);

            if let Some(next_state) = take(",", state.clone()) {
                state = next_state;
            }
        }

        if let Some(state) = take(")", state.clone()) {
            Some((args, state.clone()))
        } else {
            None
        }
    } else {
        None
    }
}

fn skip_all(token: &str, state: State) -> State {
    if state.len() > 0 && state.get(0).value == token {
        skip_all(token, state.rest())
    } else {
        state.clone()
    }
}

fn parse_func_expr(state: State) -> Possibly<(FuncExpr, State), LocError> {
    if state.limited {
        return Possibly::None;
    }

    let state = skip_all("\n", state.clone());

    let (args, state) = match parse_func_args(state.clone()) {
        Possibly::Some((args, state)) => (args, state),
        Possibly::None => (Vec::new(), state),
        Possibly::Err(e) => return Possibly::Err(e),
    };

    let state = skip_all("\n", state);

    if let Some(state) = take("{", state) {
        let state = skip_all("\n", state);
        state.indent();
        let (expressions, state) = parse_expr_list(state);
        state.dedent();
        let state = skip_all("\n", state);

        if let Some(state) = take("}", state) {
            let state = skip_all("\n", state);
            Some((
                FuncExpr {
                    args,
                    expressions: expressions.clone(),
                },
                state,
            ))
        } else {
            None
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use pretty_assertions_sorted::assert_eq;

    #[test]
    pub fn it_can_construct_an_ast() {
        let ast = Program::new(vec![]);

        assert_eq!(ast, Program::new(vec![]));
    }

    #[test]
    pub fn it_can_parse_an_empty_program() {
        let ast = parse(lex("")).unwrap();

        assert_eq!(ast, Program::new(vec![]));
    }

    #[test]
    pub fn it_can_parse_a_string() {
        let ast = parse_str(String::from("\"Hello world\"")).unwrap();

        assert_eq!(ast, String::from("Hello world"));
    }

    #[test]
    pub fn it_can_parse_a_string_expression() {
        let (expression, _state) = parse_next_expr(State::new(vec![Token::from(
            "\"Hello world\"",
            Loc::new(1, 1),
        )]))
        .unwrap();

        assert_eq!(expression, Expression::String(String::from("Hello world")));
    }

    #[test]
    pub fn it_can_parse_an_int_expression() {
        let (expression, _state) =
            parse_next_expr(State::new(vec![Token::from("123", Loc::new(1, 1))])).unwrap();

        assert_eq!(expression, Expression::Int(123));
    }

    #[test]
    pub fn it_can_parse_a_unary_func_call() {
        let (expression, state) = parse_next_expr(State::new(vec![
            Token::from("foo", Loc::new(1, 1)),
            Token::from("123", Loc::new(1, 5)),
        ]))
        .unwrap();

        assert_eq!(
            expression,
            Expression::FuncCall(FuncCall {
                ident: Variable {
                    ident: String::from("foo"),
                    loc: Loc::new(1, 1),
                },
                arg: Box::new(Expression::Int(123)),
            })
        );

        assert_eq!(state, State::new(Vec::new()));
    }

    #[test]
    pub fn it_can_parse_a_program_with_a_func_call() {
        let ast = parse(lex("print \"Hello world\"")).unwrap();

        assert_eq!(
            ast,
            Program::new(vec![Expression::FuncCall(FuncCall {
                ident: Variable {
                    ident: String::from("print"),
                    loc: Loc::new(1, 1),
                },
                arg: Box::new(Expression::String(String::from("Hello world"))),
            })])
        );
    }

    #[test]
    pub fn it_can_parse_a_program_with_multiple_func_calls() {
        let tokens = lex("print \"Hello world\" print \"Foo bar\"");

        assert_eq!(tokens[0], Token::from("print", Loc::new(1, 1)));

        let ast = parse(tokens).unwrap();

        assert_eq!(
            ast,
            Program::new(vec![
                Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(1, 1),
                    },
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                }),
                Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(1, 21),
                    },
                    arg: Box::new(Expression::String(String::from("Foo bar"))),
                }),
            ],)
        );
    }

    #[test]
    pub fn it_can_parse_a_binary_func_call() {
        let (func_call, _state) =
            parse_func_call(State::new(lex("print(\"Hello\", \"world\")"))).unwrap();

        assert_eq!(
            func_call,
            FuncCall {
                ident: Variable {
                    ident: String::from("print"),
                    loc: Loc::new(1, 1),
                },
                arg: Box::new(Expression::Touple(vec![
                    Expression::String(String::from("Hello")),
                    Expression::String(String::from("world")),
                ])),
            }
        );
    }

    #[test]
    pub fn it_can_parse_a_touple() {
        let (touple, _state) = parse_touple(State::new(lex("()"))).unwrap();

        assert_eq!(touple, Expression::Touple(Vec::<Expression>::new()));
    }

    #[test]
    pub fn it_can_parse_a_touple_with_expressions() {
        let (touple, _state) = parse_touple(State::new(lex("(\"foo\" \"bar\")"))).unwrap();

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
        let (assignment, _state) = parse_var_decl(State::new(lex("let x = 10"))).unwrap();

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
        let (func_call, _state) = parse_func_call(State::new(lex("print x"))).unwrap();

        assert_eq!(
            func_call,
            FuncCall {
                ident: Variable {
                    ident: String::from("print"),
                    loc: Loc::new(1, 1)
                },
                arg: Box::new(Expression::Variable(Variable::new(Token::from(
                    "x",
                    Loc::new(1, 7),
                )))),
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
            Program::new(vec![
                Expression::VarDecl(Box::new(VarDecl {
                    ident: String::from("x"),
                    expr: Expression::Int(10),
                })),
                Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(3, 13)
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "x",
                        Loc::new(3, 19)
                    )))),
                })
            ],)
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
            Program::new(vec![
                Expression::VarDecl(Box::new(VarDecl {
                    ident: String::from("x"),
                    expr: Expression::Int(1),
                })),
                Expression::ReAssignment(Box::new(ReAssignment {
                    ident: String::from("x"),
                    expr: Expression::Int(2),
                })),
                Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(4, 13)
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "x",
                        Loc::new(4, 19)
                    )))),
                })
            ],)
        )
    }

    #[test]
    pub fn you_can_create_a_func_expr() {
        let source = r#"
            {}
        "#;

        let (func_expr, state) = parse_func_expr(State::new(lex(source))).unwrap();

        assert_eq!(
            func_expr,
            FuncExpr {
                args: Vec::new(),
                expressions: Vec::new(),
            }
        );

        assert_eq!(state, State::new(Vec::new()));
    }

    #[test]
    pub fn you_can_create_a_func_expr_with_expressions() {
        let source = r#"
            {
                print "Hello world"
            }
        "#;

        let (func_expr, state) = parse_func_expr(State::new(lex(source))).unwrap();

        assert_eq!(
            func_expr,
            FuncExpr {
                args: Vec::new(),
                expressions: vec![Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(3, 17)
                    },
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                })],
            }
        );

        assert_eq!(state, State::new(Vec::new()));
    }

    #[test]
    pub fn you_can_create_a_func_expr_with_args() {
        let source = r#"
            (x, y) { print "Hello world" }
        "#;

        let program = parse(lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::FuncExpr(FuncExpr {
                args: vec![String::from("x"), String::from("y")],
                expressions: vec![Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(2, 22)
                    },
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                })],
            }),])
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
            Program::new(vec![
                Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: Variable::new(Token::from("print", Loc::new(2, 13))).ident,
                        loc: Loc::new(2, 13)
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "a",
                        Loc::new(2, 19),
                    )))),
                }),
                Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(3, 13)
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "b",
                        Loc::new(3, 19),
                    )))),
                }),
            ],)
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
            LocError {
                message: String::from("Unexpected token '{'"),
                loc: Loc::new(2, 13),
            }
        );
    }

    #[test]
    pub fn it_can_parse_a_simple_if_expression() {
        let source = r#"
            if true {}
        "#;

        let program = parse(lex(source));

        if let Err(err) = program {
            eprintln!(
                "{}",
                SyntaxError::generate(err, String::from(source)).message
            );
            panic!("Failed to parse program")
        }

        let program = program.unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::IfExpr(Box::new(IfExpr {
                condition: Expression::Variable(Variable::new(Token::from(
                    "true",
                    Loc::new(2, 16)
                ))),
                then_expr: Expression::FuncExpr(FuncExpr {
                    args: Vec::new(),
                    expressions: Vec::new(),
                }),
                else_expr: Option::None,
            }))])
        );
    }

    #[test]
    pub fn it_can_parse_if_expressions() {
        let source = r#"
            if true {
                print "Hello world"
            }
        "#;

        let program = parse(lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::IfExpr(Box::new(IfExpr {
                condition: Expression::Variable(Variable::new(Token::from(
                    "true",
                    Loc::new(2, 16)
                ))),
                then_expr: Expression::FuncExpr(FuncExpr {
                    args: Vec::new(),
                    expressions: vec![Expression::FuncCall(FuncCall {
                        ident: Variable {
                            ident: String::from("print"),
                            loc: Loc::new(3, 17)
                        },
                        arg: Box::new(Expression::String(String::from("Hello world"))),
                    })],
                }),
                else_expr: Option::None,
            }))])
        );
    }

    #[test]
    pub fn it_errors_on_if_at_eof() {
        let source = r#"
            if
        "#;

        let opt = parse(lex(source));

        if opt.is_ok() {
            println!("{:#?}", opt);
        }

        let result = opt.err().unwrap();

        assert_eq!(
            LocError {
                message: String::from("Unexpected token 'if'"),
                loc: Loc::new(2, 13),
            },
            result,
        );
    }
}
