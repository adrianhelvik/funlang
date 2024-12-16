use crate::types::*;

type ParseError = Option<LocError>;
type ParseResult<T> = Result<(T, State), ParseError>;
type ER = ParseResult<Expression>;

macro_rules! either {
    ($state:expr, $($func:ident),+ $(,)?) => {
        let state = $state;
        $(
            if let Some((value, state)) = allow_empty($func(&state))? {
                return Ok((value, state));
            }
        )+
    };
}

fn parse_access_target_expr(state: &State) -> ER {
    either!(
        state,
        parse_str,
        parse_int,
        parse_touple,
        parse_list,
        parse_variable,
    );

    empty()
}

/// Very simple expressions can be passed to functions.
fn parse_func_call_param_expr(state: &State) -> ER {
    either!(
        state,
        parse_str,
        parse_int,
        parse_access,
        parse_func_call,
        parse_touple,
        parse_list,
        parse_variable,
    );

    empty()
}

/// Simple expressions can be used in for-, while, and if conditions
/// and anywhere else.
fn parse_condition_expr(state: &State) -> ER {
    either!(
        state,
        parse_str,
        parse_int,
        parse_access,
        parse_touple,
        parse_list,
        parse_func_call,
        parse_variable,
    );
    empty()
}

/// Normal expression context. Available within touples for the special contexts.
fn parse_next_expr(state: &State) -> ER {
    if state.len() == 0 {
        return Err(None);
    };

    either!(
        state,
        parse_str,
        parse_int,
        parse_if_expr,
        parse_for_expr,
        parse_while_expr,
        parse_var_decl,
        parse_reassignment,
        parse_access,
        parse_str,
        parse_return_expr,
        parse_func_expr,
        parse_func_call,
        parse_variable,
        parse_list,
    );

    empty()
}

#[inline]
fn empty<T>() -> Result<T, ParseError> {
    return Err(None);
}

pub fn on_empty<Value, Producer>(
    result: Result<Value, ParseError>,
    create_err: Producer,
) -> Result<Value, ParseError>
where
    Producer: Fn() -> LocError,
{
    match result {
        Ok(t) => Ok(t),
        Err(possibly_loc_err) => match possibly_loc_err {
            None => Err(Some(create_err())),
            Some(loc_err) => Err(Some(loc_err)),
        },
    }
}

fn transform_empty<T>(result: Result<T, ParseError>, default_value: T) -> Result<T, LocError> {
    match result {
        Err(Some(err)) => Err(err),
        Err(None) => Ok(default_value),
        Ok(value) => Ok(value),
    }
}

pub fn parse(input: &Tokens) -> Result<Program, LocError> {
    let state = State::new(input.clone());

    parse_state(state)
}

fn parse_state(state: State) -> Result<Program, LocError> {
    let (expressions, state) =
        transform_empty(parse_expr_list(&state), (Vec::<Expression>::new(), state))?;

    let state = skip_all("\n", state);

    if state.len() > 0 {
        let token = state.get(0);
        return Err(token
            .loc
            .error(format!("Unexpected token '{}'", token.value).as_ref()));
    }

    Ok(Program::new(expressions))
}

#[derive(Debug, PartialEq)]
struct State {
    tokens: Tokens,
    final_loc: Loc,
}

impl State {
    pub fn new(tokens: Tokens) -> Self {
        let final_loc = match tokens.last() {
            Some(token) => token.loc.clone(),
            None => Loc { line: 1, column: 1 },
        };
        State { tokens, final_loc }
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
            final_loc: self.final_loc.clone(),
        }
    }

    pub fn take_first(&self) -> Result<(Token, State), ()> {
        if self.tokens.len() == 0 {
            return Err(());
        }
        let mut tokens = self.tokens.clone();
        let first = tokens.remove(0);

        return Ok((
            first,
            State {
                tokens,
                final_loc: self.final_loc.clone(),
            },
        ));
    }

    fn error(&self, message: &str) -> ParseError {
        Some(LocError {
            message: message.to_string(),
            loc: match self.tokens.get(0) {
                Some(token) => token.loc.clone(),
                None => self.final_loc.clone(),
            },
        })
    }
}

fn parse_expr_list(state: &State) -> ParseResult<Vec<Expression>> {
    let state = skip_all("\n", state.clone());

    let mut expressions = vec![];
    let mut state = state.clone();
    let mut remaining_state = state.len();

    while let Some((expr, next_state)) = allow_empty(parse_next_expr_allow_newline(&state))? {
        expressions.push(expr);
        state = next_state;

        if state.len() == remaining_state {
            panic!("Infinite loop in parse()");
        }

        remaining_state = state.len();
    }

    Ok((expressions, state.clone()))
}

fn parse_comma_expr_list(state: State) -> ParseResult<Vec<Expression>> {
    let state = skip_all("\n", state);

    let mut expressions = vec![];
    let mut state = state.clone();
    let mut remaining_state = state.len();

    while let Some((expr, next_state)) = allow_empty(parse_next_expr_allow_newline(&state))? {
        expressions.push(expr);
        state = next_state;

        if state.len() == remaining_state {
            panic!("Infinite loop in parse()");
        }

        remaining_state = state.len();

        match take(",", &state) {
            Err(err) => {
                return match err {
                    Some(err) => Err(Some(err)),
                    None => Ok((expressions, state.clone())),
                }
            }
            Ok(next_state) => {
                state = next_state;
            }
        };
    }

    Ok((expressions, state.clone()))
}

fn require(expected: &str, state: &State, message: &str) -> Result<State, ParseError> {
    let (token, state) = state.take_first().map_err(|()| None)?;
    if token.value == expected {
        Ok(state)
    } else {
        Err(state.error(&format!(
            "{} (expected '{}', got '{}')",
            message, expected, &token.value
        )))
    }
}

fn parse_var_decl(state: &State) -> ER {
    let (let_token, state) = pick("let", &state)?;

    let (ident, state) = take_ident_token(&state)?;

    let state = require("=", &state, "Expected identifier after let keyword")?;

    let (expr, state) = parse_next_expr(&state)?;

    Ok((
        Expression::VarDecl(Box::new(VarDecl {
            ident: ident.value,
            expr,
            loc: let_token.loc.clone(),
        })),
        state,
    ))
}

fn parse_if_expr(state: &State) -> ER {
    let (if_kw, state) = pick("if", state)?;

    let (condition, state) = match parse_condition_expr(&state) {
        Ok((condition, state)) => (condition, state),
        Err(Some(err)) => return Err(Some(err)),
        Err(None) => {
            return Err(Some(
                if_kw.loc.error("Expected expression after if keyword"),
            ))
        }
    };

    let (then_expr, state) = on_empty(parse_next_expr(&state), || {
        if_kw.loc.error("Expected expression after if condition")
    })?;

    let state = skip_all("\n", state);

    let (else_kw, state) = match pick("else", &state) {
        Err(e) => {
            return match e {
                Some(e) => Err(Some(e)),
                None => Ok((
                    Expression::IfExpr(Box::new(IfExpr {
                        condition,
                        then_expr,
                        else_expr: None,
                    })),
                    state,
                )),
            };
        }
        other => other,
    }?;

    let (opening_brace, state) = match pick("{", &state) {
        Ok(state) => Ok(state),
        Err(e) => Err(match e {
            Some(e) => Some(e),
            None => Some(LocError {
                message: "Expected opening brace after else keyword".to_string(),
                loc: else_kw.loc,
            }),
        }),
    }?;

    let (else_expr, state) = match parse_next_expr(&state) {
        Ok(value) => Ok(value),
        Err(err) => Err(match err {
            Some(e) => Some(e),
            None => Some(LocError {
                message: "Expected expression after else keyword".to_string(),
                loc: opening_brace.loc,
            }),
        }),
    }?;

    let state = require("}", &state, "Closing brace is required")?;

    Ok((
        Expression::IfExpr(Box::new(IfExpr {
            condition,
            then_expr,
            else_expr: Option::Some(else_expr),
        })),
        state,
    ))
}

fn parse_for_expr(state: &State) -> ER {
    let state = take("for", &state)?;
    let (ident, state) = take_ident_token(&state)?;
    let state = require("in", &state, "Missing 'in' keyword in for-loop")?;
    let (start_expr, state) = parse_condition_expr(&state)?;
    println!("{:#?}", start_expr);
    let state = take("..", &state)?;
    let (end_expr, state) = parse_condition_expr(&state)?;
    let state = require("{", &state, "Missing opening brace")?;
    let (expressions, state) = parse_expr_list(&state)?;
    let state = skip_all("\n", state);
    let state = require("}", &state, "Missing closing brace in for-loop")?;

    let for_expr = ForExpr {
        identifier: ident.to_variable(),
        start: Box::new(start_expr),
        end: Box::new(end_expr),
        body: expressions,
    };

    return Ok((Expression::ForExpr(for_expr), state));
}

fn parse_while_expr(state: &State) -> ER {
    let state = take("while", &state)?;
    let (condition, state) = parse_condition_expr(&state)?;
    let state = require("{", &state, "Missing opening brace in while-loop")?;
    let (expressions, state) = parse_expr_list(&state)?;
    let state = skip_all("\n", state);
    let state = require("}", &state, "Missing closing brace in while-loop")?;

    let while_expr = WhileExpr {
        condition: Box::new(condition),
        body: expressions,
    };

    Ok((Expression::WhileExpr(while_expr), state))
}

fn take_identifier(state: &State) -> ParseResult<(Identifier, Loc)> {
    let (first, state) = take_ident_token(state)?;
    let mut identifiers = vec![first];
    let mut state = state;
    while let Some(next_state) = allow_empty(take(".", &state))? {
        state = next_state.into();
        let (successive, next_state) = take_ident_token(&state)?;
        state = next_state.into();
        identifiers.push(successive);
    }

    if identifiers.len() > 1 {
        let strings = identifiers.iter().map(|token| token.value.clone()).collect();
        return Ok(((Identifier::DotAccess(strings), identifiers[0].loc.clone()), state));
    }

    Ok((
        (
            Identifier::Literal(identifiers[0].value.clone()),
            identifiers[0].loc.clone(),
        ),
        state,
    ))
}

fn parse_reassignment(state: &State) -> ER {
    let ((ident, loc), state) = take_identifier(state)?;
    let state = take("=", &state)?;
    let (expr, state) = parse_next_expr(&state)?;

    Ok((
        Expression::ReAssignment(Box::new(ReAssignment { ident, expr, loc })),
        state,
    ))
}

fn take(expected: &str, state: &State) -> Result<State, ParseError> {
    let (token, state) = state.take_first().map_err(|()| None)?;
    if token.value == expected {
        Ok(state)
    } else {
        Err(None)
    }
}

fn pick(expected: &str, state: &State) -> Result<(Token, State), ParseError> {
    let (token, state) = state.take_first().map_err(|()| None)?;
    if token.value == expected {
        Ok((token, state))
    } else {
        Err(None)
    }
}

fn parse_touple(state: &State) -> Result<(Expression, State), ParseError> {
    let state = take("(", state)?;
    let (expressions, state) = parse_comma_expr_list(state)?;
    let state = require(")", &state, "Missing closing parenthesis for touple")?;

    Ok((Expression::Touple(expressions), state))
}

fn parse_str(state: &State) -> Result<(Expression, State), ParseError> {
    let (token, state) = state.take_first().map_err(|()| None)?;
    let mut value = String::new();

    for (i, c) in token.value.chars().enumerate() {
        if i == 0 && c != '"' {
            return Err(None);
        }
        if i > 0 {
            value.push(c);
        }
    }

    value.pop();

    Ok((Expression::String(value), state))
}

fn take_ident_token(state: &State) -> ParseResult<Token> {
    let (token, state) = state.take_first().map_err(|()| None)?;

    if is_ident(&token) {
        Ok((token, state))
    } else {
        Err(None)
    }
}

fn require_ident(state: &State) -> ParseResult<Token> {
    let (token, state) = state
        .take_first()
        .map_err(|()| state.final_loc.error("Expected identifier"))?;

    if is_ident(&token) {
        Ok((token, state))
    } else {
        Err(Some(token.loc.error("Expected identifier")))
    }
}

fn is_ident(token: &Token) -> bool {
    match token.value.as_ref() {
        "\n" | "(" | ")" | "{" | "}" | "[" | "]" | "," | ".." | "=" => false,
        _ => true,
    }
}

fn parse_func_call(state: &State) -> Result<(Expression, State), ParseError> {
    let (token, state) = take_ident_token(state)?;
    let (arg, state) = parse_func_call_param_expr(&state)?;

    Ok((
        Expression::FuncCall(FuncCall {
            ident: Variable {
                ident: token.value,
                loc: token.loc,
            },
            arg: Box::new(arg),
        }),
        state,
    ))
}

fn parse_int(state: &State) -> ER {
    let (token, state) = state.take_first().map_err(|()| None)?;
    let mut sum = 0;
    let len = token.value.len();

    for (i, c) in token.value.chars().enumerate() {
        let exponent = u32::try_from(len - i - 1).unwrap();
        if let Some(digit) = digit_to_int(c) {
            sum += digit * (10 as i64).pow(exponent);
        } else {
            return Err(None);
        }
    }

    Ok((Expression::Int(sum), state))
}

fn parse_access(state: &State) -> ER {
    let (first, state) = parse_access_target_expr(state)?;
    let (dot_token, state) = pick(".", &state)?;
    let (second, state) = require_ident(&state)?;

    Ok((
        Expression::Access(Access {
            loc: dot_token.loc.clone(),
            target: Box::new(first),
            key: second.value,
        }),
        state,
    ))
}

fn parse_next_expr_allow_newline(state: &State) -> Result<(Expression, State), ParseError> {
    let state = skip_all("\n", state.clone());
    parse_next_expr(&state)
}

fn allow_empty<T>(input: Result<T, ParseError>) -> Result<Option<T>, ParseError> {
    match input {
        Err(Some(err)) => Err(Some(err)),
        Err(None) => Ok(None),
        Ok(value) => Ok(Some(value)),
    }
}

fn parse_variable(state: &State) -> ER {
    let (token, state) = take_ident_token(state)?;
    return Ok((Expression::Variable(Variable::new(token)), state));
}

fn parse_list(state: &State) -> Result<(Expression, State), ParseError> {
    let (opening_bracket, state) = pick("[", state)?;
    let (items, state) = parse_comma_expr_list(state)?;
    let state = require("]", &state, "Unterminated list")?;

    let expr = Expression::List(List {
        items,
        loc: opening_bracket.loc.clone(),
    });

    return Ok((expr, state));
}

fn parse_return_expr(state: &State) -> ER {
    let state = skip_all("\n", state.clone());
    let state = match allow_empty(take("return", &state))? {
        Some(value) => value,
        None => take("ret", &state)?,
    };
    let state = skip_all("\n", state.clone());
    let (expr, state) = parse_next_expr(&state)?;
    Ok((Expression::Return(Box::new(expr)), state))
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

fn parse_func_args(state: &State) -> ParseResult<Vec<String>> {
    let mut state = take("(", state)?;
    let mut args = Vec::<String>::new();

    loop {
        state = match take_ident_token(&state) {
            Ok((ident, state)) => {
                args.push(ident.value);
                state
            }
            Err(err) => match err {
                Some(err) => {
                    return Err(Some(err));
                }
                None => {
                    break;
                }
            },
        };
        state = match take(",", &state) {
            Ok(state) => state,
            Err(err) => match err {
                Some(err) => {
                    return Err(Some(err));
                }
                None => {
                    break;
                }
            },
        };
    }

    let state = skip_all("\n", state);
    let state = require(")", &state, "Missing closing parenthesis for function args")?;

    return Ok((args, state));
}

fn skip_all(expected: &str, initial_state: State) -> State {
    match initial_state.take_first() {
        Ok((token, new_state)) => {
            if token.value != expected {
                return initial_state;
            }
            skip_all(expected, new_state)
        }
        Err(()) => initial_state,
    }
}

fn parse_func_expr(state: &State) -> ER {
    let state = skip_all("\n", state.clone());

    let (args, state) = match allow_empty(parse_func_args(&state))? {
        Some((args, state)) => (args, state),
        None => (vec![], state),
    };

    let state = skip_all("\n", state);

    let state = take("{", &state)?;

    let state = skip_all("\n", state);
    let (expressions, state) = parse_expr_list(&state)?;
    let state = skip_all("\n", state);

    let state = require(
        "}",
        &state,
        "Expected closing brace for function expression",
    )?;
    let state = skip_all("\n", state);

    Ok((
        Expression::FuncExpr(FuncExpr {
            args,
            expressions: expressions.clone(),
        }),
        state,
    ))
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
        let ast = parse(&lex("")).unwrap();

        assert_eq!(ast, Program::new(vec![]));
    }

    #[test]
    pub fn it_can_parse_a_string() {
        let (string, state) = parse_str(&State::new(lex("\"Hello world\""))).unwrap();

        assert_eq!(string, Expression::String(String::from("Hello world")));
        assert_eq!(state.tokens.len(), 0);
    }

    #[test]
    pub fn it_can_parse_a_string_expression() {
        let (expression, _state) = parse_next_expr(&State::new(vec![Token::from(
            "\"Hello world\"",
            Loc::new(1, 1),
        )]))
        .unwrap();

        assert_eq!(expression, Expression::String(String::from("Hello world")));
    }

    #[test]
    pub fn it_can_parse_an_int_expression() {
        let (expression, _state) =
            parse_next_expr(&State::new(vec![Token::from("123", Loc::new(1, 1))])).unwrap();

        assert_eq!(expression, Expression::Int(123));
    }

    #[test]
    pub fn it_can_parse_a_unary_func_call() {
        let (expression, state) = parse_next_expr(&State::new(vec![
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

        assert_eq!(
            state,
            State {
                tokens: vec![],
                final_loc: Loc::new(1, 5),
            }
        );
    }

    #[test]
    pub fn it_can_parse_a_program_with_a_func_call() {
        let ast = parse(&lex("print \"Hello world\"")).unwrap();

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
        let tokens = lex("print \"Hello world\"\nprint \"Foo bar\"");

        assert_eq!(tokens[0], Token::from("print", Loc::new(1, 1)));

        let ast = parse(&tokens).unwrap();

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
                        loc: Loc::new(2, 1),
                    },
                    arg: Box::new(Expression::String(String::from("Foo bar"))),
                }),
            ],)
        );
    }

    #[test]
    pub fn funky_same_line_multiple_function_calls() {
        let tokens = lex("print \"Hello world\" print \"Foo bar\"");

        assert_eq!(tokens[0], Token::from("print", Loc::new(1, 1)));

        let ast = parse(&tokens).unwrap();

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
            parse_func_call(&State::new(lex("print(\"Hello\", \"world\")"))).unwrap();

        assert_eq!(
            func_call,
            Expression::FuncCall(FuncCall {
                ident: Variable {
                    ident: String::from("print"),
                    loc: Loc::new(1, 1),
                },
                arg: Box::new(Expression::Touple(vec![
                    Expression::String(String::from("Hello")),
                    Expression::String(String::from("world")),
                ])),
            })
        );
    }

    #[test]
    pub fn it_can_parse_a_touple() {
        let (touple, _state) = parse_touple(&State::new(lex("()"))).unwrap();

        assert_eq!(touple, Expression::Touple(Vec::<Expression>::new()));
    }

    #[test]
    pub fn it_can_parse_a_touple_with_expressions() {
        let (touple, _state) = parse_touple(&State::new(lex("(\"foo\", \"bar\")"))).unwrap();

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
        let (assignment, _state) = parse_var_decl(&State::new(lex("let x = 10"))).unwrap();

        assert_eq!(
            assignment,
            Expression::VarDecl(Box::new(VarDecl {
                ident: String::from("x"),
                expr: Expression::Int(10),
                loc: Loc { line: 1, column: 1 },
            }))
        );
    }

    #[test]
    pub fn it_can_call_print_with_a_variable() {
        let (func_call, _state) = parse_func_call(&State::new(lex("print x"))).unwrap();

        assert_eq!(
            func_call,
            Expression::FuncCall(FuncCall {
                ident: Variable {
                    ident: String::from("print"),
                    loc: Loc::new(1, 1)
                },
                arg: Box::new(Expression::Variable(Variable::new(Token::from(
                    "x",
                    Loc::new(1, 7),
                )))),
            })
        )
    }

    #[test]
    pub fn you_can_assign_variables() {
        let source = r#"let x = 10"#;

        assert_eq!(
            parse(&lex(source)).unwrap(),
            Program::new(vec![Expression::VarDecl(Box::new(VarDecl {
                ident: String::from("x"),
                expr: Expression::Int(10),
                loc: Loc { line: 1, column: 1 },
            })),],)
        )
    }

    #[test]
    fn simple_reassignment() {
        let source = "x = 1";

        assert_eq!(
            parse(&lex(source)).unwrap(),
            Program::new(vec![Expression::ReAssignment(Box::new(ReAssignment {
                ident: Identifier::Literal(String::from("x")),
                expr: Expression::Int(1),
                loc: Loc { line: 1, column: 1 },
            })),])
        );
    }

    #[test]
    pub fn you_can_reassign_variables() {
        let source = r#"
            let x = 1
            x = 2
            print x
        "#;

        assert_eq!(
            parse(&lex(source)).unwrap(),
            Program::new(vec![
                Expression::VarDecl(Box::new(VarDecl {
                    ident: String::from("x"),
                    expr: Expression::Int(1),
                    loc: Loc {
                        line: 2,
                        column: 13,
                    },
                })),
                Expression::ReAssignment(Box::new(ReAssignment {
                    ident: Identifier::Literal(String::from("x")),
                    expr: Expression::Int(2),
                    loc: Loc {
                        line: 3,
                        column: 13,
                    },
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

        let (func_expr, state) = parse_func_expr(&State::new(lex(source))).unwrap();

        assert_eq!(
            func_expr,
            Expression::FuncExpr(FuncExpr {
                args: Vec::new(),
                expressions: Vec::new(),
            })
        );

        assert_eq!(
            state,
            State {
                tokens: vec![],
                final_loc: Loc::new(2, 15),
            }
        );
    }

    #[test]
    pub fn you_can_create_a_func_expr_with_expressions() {
        let source = r#"
            {
                print "Hello world"
            }
        "#;

        let (func_expr, state) = parse_func_expr(&State::new(lex(source))).unwrap();

        assert_eq!(
            func_expr,
            Expression::FuncExpr(FuncExpr {
                args: Vec::new(),
                expressions: vec![Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: String::from("print"),
                        loc: Loc::new(3, 17)
                    },
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                })],
            })
        );

        assert_eq!(
            state,
            State {
                tokens: vec![],
                final_loc: Loc::new(4, 14),
            }
        );
    }

    #[test]
    pub fn you_can_create_a_func_expr_with_args() {
        let source = r#"
            (x, y) { print "Hello world" }
        "#;

        let program = parse(&lex(source)).unwrap();

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

        let program = parse(&lex(source)).unwrap();

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

        let result = parse(&lex(source)).err().unwrap();

        assert_eq!(
            result,
            LocError {
                message: String::from("Unexpected token '{'"),
                loc: Loc::new(2, 13),
            }
        );
    }

    #[test]
    fn it_can_parse_true() {
        let source = "true";
        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::Variable(Variable {
                ident: "true".to_string(),
                loc: Loc::new(1, 1),
            })])
        );
    }

    #[test]
    pub fn it_can_parse_a_simple_if_expression() {
        let source = r#"
            if true {}
        "#;

        let program = parse(&lex(source)).unwrap();

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

        let program = parse(&lex(source)).unwrap();

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

        let opt = parse(&lex(source));

        if opt.is_ok() {
            println!("{:#?}", opt);
        }

        let result = opt.err().unwrap();

        assert_eq!(
            LocError {
                message: String::from("Expected expression after if keyword"),
                loc: Loc::new(2, 13),
            },
            result,
        );
    }

    #[test]
    pub fn it_can_parse_a_return_statement() {
        let source = r#"
            return "Hello world"
        "#;

        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::Return(Box::new(Expression::String(
                "Hello world".to_string()
            )))])
        );
    }

    #[test]
    pub fn it_can_parse_a_function_with_a_return() {
        let source = r#"
            () {
                return "Hello world"
            }
        "#;

        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::FuncExpr(FuncExpr {
                args: vec![],
                expressions: vec![Expression::Return(Box::new(Expression::String(
                    "Hello world".to_string()
                )))]
            })])
        );
    }

    #[test]
    fn it_can_parse_for_in_with_the_dedicated_parser_fn() {
        let source = r#"for i in 0..10 {123}"#;
        let tokens = lex(source);
        let state = State::new(tokens);
        let (for_expr, state) = parse_for_expr(&state).unwrap();

        assert_eq!(
            for_expr,
            Expression::ForExpr(ForExpr {
                identifier: Variable {
                    ident: "i".to_string(),
                    loc: Loc::new(1, 5),
                },
                start: Box::new(Expression::Int(0)),
                end: Box::new(Expression::Int(10)),
                body: vec![Expression::Int(123),],
            })
        );

        assert_eq!(state.tokens, vec![]);
    }

    #[test]
    fn it_can_parse_for_in_expressions() {
        let source = r#"for i in 0..10 {123}"#;

        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::ForExpr(ForExpr {
                identifier: Variable {
                    ident: String::from("i"),
                    loc: Loc { line: 1, column: 5 },
                },
                start: Box::new(Expression::Int(0)),
                end: Box::new(Expression::Int(10)),
                body: vec![Expression::Int(123),],
            })])
        );
    }

    #[test]
    pub fn it_can_parse_a_while_loop() {
        let source = r#"while lt(i, 10) {123}"#;

        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::WhileExpr(WhileExpr {
                condition: Box::new(Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: "lt".to_string(),
                        loc: Loc { line: 1, column: 7 },
                    },
                    arg: Box::new(Expression::Touple(vec![
                        Expression::Variable(Variable {
                            ident: "i".to_string(),
                            loc: Loc {
                                line: 1,
                                column: 10,
                            },
                        }),
                        Expression::Int(10),
                    ])),
                })),
                body: vec![Expression::Int(123),],
            })])
        );
    }

    #[test]
    pub fn it_can_parse_for_expressions_with_extra_stuff() {
        let source = r#"
            for i in 0..10 {
                123
            }
            let x = 10
            "#;

        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program::new(vec![
                Expression::ForExpr(ForExpr {
                    identifier: Variable {
                        ident: String::from("i"),
                        loc: Loc {
                            line: 2,
                            column: 17
                        },
                    },
                    start: Box::new(Expression::Int(0)),
                    end: Box::new(Expression::Int(10)),
                    body: vec![Expression::Int(123),],
                }),
                Expression::VarDecl(Box::new(VarDecl {
                    ident: String::from("x"),
                    expr: Expression::Int(10),
                    loc: Loc {
                        line: 5,
                        column: 13,
                    },
                })),
            ])
        );
    }

    #[test]
    pub fn it_can_parse_lists() {
        let source = r#"[5, 7, 13, 29]"#;

        let ast = parse(&lex(source)).unwrap();

        assert_eq!(
            ast,
            Program {
                expressions: vec![Expression::List(List {
                    loc: Loc { line: 1, column: 1 },
                    items: vec![
                        Expression::Int(5),
                        Expression::Int(7),
                        Expression::Int(13),
                        Expression::Int(29),
                    ],
                }),],
            }
        );
    }

    #[test]
    fn it_can_parse_function_args() {
        let source = "(a, b, c) {\n}";

        let ast = parse(&lex(source)).unwrap();

        assert_eq!(
            ast,
            Program {
                expressions: vec![Expression::FuncExpr(FuncExpr {
                    args: vec!["a".to_string(), "b".to_string(), "c".to_string()],
                    expressions: vec![],
                })],
            },
        );
    }

    #[test]
    fn ret_return() {
        let source = r#"
            let x = () { ret 1 }
        "#;

        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program {
                expressions: vec![Expression::VarDecl(Box::new(VarDecl {
                    ident: "x".to_string(),
                    loc: Loc::new(2, 13),
                    expr: Expression::FuncExpr(FuncExpr {
                        args: vec![],
                        expressions: vec![Expression::Return(Box::new(Expression::Int(1))),],
                    })
                }))],
            }
        );
    }

    #[test]
    fn nested_func_calls() {
        let source = r#"eq(1, x())"#;

        let program = parse(&lex(source)).unwrap();

        assert_eq!(
            program,
            Program {
                expressions: vec![Expression::FuncCall(FuncCall {
                    ident: Variable {
                        ident: "eq".to_string(),
                        loc: Loc { line: 1, column: 1 },
                    },
                    arg: Box::new(Expression::Touple(vec![
                        Expression::Int(1),
                        Expression::FuncCall(FuncCall {
                            ident: Variable {
                                ident: "x".to_string(),
                                loc: Loc { line: 1, column: 7 },
                            },
                            arg: Box::new(Expression::Touple(vec![]))
                        })
                    ]))
                })]
            }
        );
    }

    mod dot_notation {
        use super::*;
        use pretty_assertions_sorted::assert_eq;

        #[test]
        fn access() {
            let source = r#"foo.bar"#;

            let ast = parse(&lex(source)).unwrap();

            assert_eq!(
                ast,
                Program {
                    expressions: vec![Expression::Access(Access {
                        loc: Loc::new(1, 4),
                        target: Box::new(Expression::Variable(Variable {
                            loc: Loc::new(1, 1),
                            ident: "foo".to_string(),
                        })),
                        key: "bar".to_string(),
                    }),],
                },
            );
        }

        #[test]
        fn assignment() {
            let source = r#"foo.bar = 123"#;

            let ast = parse(&lex(source)).unwrap();

            assert_eq!(
                ast,
                Program {
                    expressions: vec![Expression::ReAssignment(Box::new(ReAssignment {
                        loc: Loc::new(1, 1),
                        ident: Identifier::DotAccess(vec!["foo".to_string(), "bar".to_string(),]),
                        expr: Expression::Int(123),
                    }))],
                },
            );
        }

        /*
        #[test]
        fn method_call() {
            let source = r#"foo.bar()"#;

            let ast = parse(&lex(source)).unwrap();

            assert_eq!(
                ast,
                Program {
                    expressions: vec![
                        Expression::FuncCall(FuncCall {
                            ident: Identifier::DotAccess(vec!["foo".to_string(), "bar".to_string()]),
                            arg: Box::new(Expression::Touple(vec![])),
                        })
                    ]
                },
            );
        }
*/
    }
}
