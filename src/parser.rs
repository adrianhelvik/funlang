use crate::util::Optionality::{self, *};
use crate::{parse_one_of, types::*};
use std::{cell::RefCell, rc::Rc};

type ParseError = Option<LocError>;
type ParseResult<T> = Result<(T, State), ParseError>;
type ExprRes = ParseResult<LocExpr>;

fn parse_simple_touple_arg(state: &State) -> ExprRes {
    parse_one_of!(
        state,
        parsers: [
            parse_str,
            parse_int,
            parse_func_call,
            parse_variable,
        ]
    );

    empty()
}

fn parse_func_call_target(state: &State) -> ExprRes {
    parse_one_of!(
        state,
        parsers: [
            parse_touple,
            parse_access,
            parse_variable,
        ]
    );

    empty()
}

fn parse_func_arg_expr(state: &State) -> ExprRes {
    if state.is_in_block_arg {
        return parse_block_arg_expr(state);
    }

    parse_one_of!(
        state,
        parsers: [
            parse_simple_touple,
            parse_str,
            parse_int,
            parse_func_expr_in_func_call_context,
            parse_func_call,
            parse_touple,
            parse_list,
            parse_access,
            parse_variable,
        ]
    );

    empty()
}

fn parse_block_arg_expr(state: &State) -> ExprRes {
    let state = &state.in_block_arg();

    parse_one_of!(
        state,
        transform_state_after: |state: State| state.not_in_block_arg(),
        parsers: [
            parse_str,
            parse_int,
            parse_func_call,
            parse_touple,
            parse_list,
            parse_access,
            parse_variable,
        ]
    );

    empty()
}

fn parse_next_expr(state: &State) -> ExprRes {
    parse_one_of!(
        state,
        parsers: [
            parse_import_expr,
            parse_if_expr,
            parse_for_expr,
            parse_while_expr,
            parse_var_decl,
            parse_reassignment,
            parse_return_expr,
            parse_func_expr,
            parse_func_call,
            parse_list,
            parse_str,
            parse_int,
            parse_touple,
            parse_list,
            parse_access,
            parse_variable,
        ]
    );

    empty()
}

#[inline]
fn empty<T>() -> Result<T, ParseError> {
    return Err(None);
}

pub fn fail_if_empty<Value, Producer>(
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

    state.parse()
}

#[derive(Debug, Clone, PartialEq)]
struct State {
    tokens: Tokens,
    final_loc: Loc,
    is_in_block_arg: bool,
}

impl State {
    #[cfg(test)]
    pub fn with_final_loc(line: usize, column: usize) -> State {
        let mut state = State::new(vec![]);
        state.final_loc = Loc::new(line, column);
        state
    }

    pub fn from(tokens: &Tokens) -> Self {
        Self::new(tokens.clone())
    }

    pub fn new(tokens: Tokens) -> Self {
        let final_loc = match tokens.last() {
            Some(token) => token.loc.clone(),
            None => Loc { line: 1, column: 1 },
        };
        State {
            tokens,
            final_loc,
            is_in_block_arg: false,
        }
    }

    pub fn in_block_arg(&self) -> State {
        let mut state = self.clone();
        state.is_in_block_arg = true;
        state
    }

    pub fn not_in_block_arg(&self) -> State {
        let mut state = self.clone();
        state.is_in_block_arg = false;
        state
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn get(&self, index: usize) -> Token {
        self.tokens.get(index).cloned().unwrap()
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
                is_in_block_arg: self.is_in_block_arg,
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

    fn next_is(&self, expected: &str) -> bool {
        return match self.tokens.first() {
            Some(token) => token.value == expected,
            None => false,
        };
    }

    fn parse(&self) -> Result<Program, LocError> {
        let (expressions, state) =
            transform_empty(parse_expr_list(self), (Vec::<LocExpr>::new(), self.clone()))?;

        let state = skip_all("\n", state);

        if state.len() > 0 {
            let token = state.get(0);
            return Err(token
                .loc
                .error(format!("Unexpected token '{}'", token.value).as_ref()));
        }

        Ok(Program::new(expressions))
    }

    fn loc(&self) -> Option<Loc> {
        self.tokens.get(0).map(|t| t.loc.clone())
    }
}

fn parse_expr_list(state: &State) -> ParseResult<Vec<LocExpr>> {
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

fn parse_comma_expr_list(state: State) -> ParseResult<Vec<LocExpr>> {
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

fn parse_var_decl(state: &State) -> ExprRes {
    let (let_token, state) = pick("let", &state)?;

    let (ident, state) = fail_if_empty(take_variable(&state), || {
        let_token
            .loc
            .error("'let' must be followed by an identifier")
    })?;

    let state = require("=", &state, "Expected identifier after let keyword")?;

    let (expr, state) = parse_next_expr(&state)?;

    Ok((
        LocExpr {
            loc: let_token.loc.clone(),
            expr: Expression::VarDecl(Box::new(VarDecl {
                ident: ident.ident,
                expr,
            })),
        },
        state,
    ))
}

fn parse_if_expr(state: &State) -> ExprRes {
    let (if_kw, state) = pick("if", state)?;

    let (condition, state) = match parse_block_arg_expr(&state) {
        Ok((condition, state)) => (condition, state),
        Err(Some(err)) => return Err(Some(err)),
        Err(None) => {
            return Err(Some(
                if_kw.loc.error("Expected expression after if keyword"),
            ))
        }
    };

    let (then_expr, state) = fail_if_empty(parse_next_expr(&state), || {
        if_kw.loc.error("Expected expression after if condition")
    })?;

    let state = skip_all("\n", state);

    let (else_kw, state) = match pick("else", &state) {
        Err(e) => {
            return match e {
                Some(e) => Err(Some(e)),
                None => Ok((
                    if_kw.loc.wrap(Expression::IfExpr(Box::new(IfExpr {
                        condition,
                        then_expr,
                        else_expr: None,
                    }))),
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
        if_kw.loc.wrap(Expression::IfExpr(Box::new(IfExpr {
            condition,
            then_expr,
            else_expr: Option::Some(else_expr),
        }))),
        state,
    ))
}

fn take_range(state: &State) -> Result<(RangeLiteral, State), ParseError> {
    let (start_expr, state) = parse_block_arg_expr(&state)?;
    let state = take("..", &state)?;
    let (end_expr, state) = parse_block_arg_expr(&state)?;

    let range = RangeLiteral {
        start: start_expr,
        end: end_expr,
    };

    Ok((range, state))
}

fn parse_for_expr(state: &State) -> ExprRes {
    let (for_kw, state) = pick("for", &state)?;
    let (ident, state) = take_variable(&state)?;
    let state = require("in", &state, "Missing 'in' keyword in for-loop")?;

    let (range, state) = take_range(&state)?;

    let state = require("{", &state, "Missing opening brace")?;
    let (expressions, state) = parse_expr_list(&state)?;
    let state = skip_all("\n", state);
    let state = require("}", &state, "Missing closing brace in for-loop")?;

    let for_expr = ForExpr {
        identifier: ident,
        range: Box::new(range),
        body: expressions,
    };

    return Ok((for_kw.loc.wrap(Expression::ForExpr(for_expr)), state));
}

fn parse_while_expr(state: &State) -> ExprRes {
    let (kw, state) = pick("while", &state)?;
    let (condition, state) = parse_block_arg_expr(&state)?;
    let state = require("{", &state, "Missing opening brace in while-loop")?;
    let (expressions, state) = parse_expr_list(&state)?;
    let state = skip_all("\n", state);
    let state = require("}", &state, "Missing closing brace in while-loop")?;

    let while_expr = WhileExpr {
        condition: Box::new(condition),
        body: expressions,
    };

    Ok((kw.loc.wrap(Expression::WhileExpr(while_expr)), state))
}

/*
fn take_identifier(state: &State) -> ParseResult<Identifier> {
    let (first, state) = take_ident_token(state)?;
    let mut identifiers = vec![first];
    let mut state = state;
    while let Some(next_state) = allow_empty(take(".", &state))? {
        state = next_state.into();
        let (successive, next_state) = take_ident_token(&state)?;
        state = next_state.into();
        identifiers.push(successive);
    }

    return Ok((
        Identifier {
            accessors: identifiers,
        },
        state,
    ));
}
*/

fn parse_reassignment(state: &State) -> ExprRes {
    parse_one_of!(
        state,
        parsers: [
            parse_access_reassignment,
            parse_var_reassignment,
        ]
    );

    empty()
}

fn parse_var_reassignment(state: &State) -> ExprRes {
    let (var, state) = take_variable(state)?;
    let loc = var.loc.clone();
    let state = take("=", &state)?;
    let (expr, state) = fail_if_empty(parse_next_expr(&state), || {
        loc.error("Assignment must be followed by an expression")
    })?;

    Ok((
        loc.wrap(Expression::ReAssignment(Box::new(ReAssignment {
            target: VarOrAccess::Variable(var),
            expr,
        }))),
        state,
    ))
}

fn parse_access_reassignment(state: &State) -> ExprRes {
    let (access, state) = take_access(state)?;
    let loc = access.loc.clone();
    let state = take("=", &state)?;
    let (expr, state) = fail_if_empty(parse_next_expr(&state), || {
        loc.error("Assignment must be followed by an expression")
    })?;

    Ok((
        loc.wrap(Expression::ReAssignment(Box::new(ReAssignment {
            target: VarOrAccess::Access(access),
            expr,
        }))),
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

fn parse_touple(state: &State) -> ExprRes {
    let was_in_block = state.is_in_block_arg;
    let state = if was_in_block {
        state.not_in_block_arg()
    } else {
        state.clone()
    };
    let (opening_paren, state) = pick("(", &state)?;
    let (expressions, state) = parse_comma_expr_list(state)?;
    let state = require(")", &state, "Missing closing parenthesis for touple")?;

    let state = if was_in_block {
        state.in_block_arg()
    } else {
        state
    };

    Ok((
        Expression::Touple(expressions).with_loc(&opening_paren.loc),
        state,
    ))
}

fn parse_simple_touple(state: &State) -> ExprRes {
    if state.is_in_block_arg {
        return empty();
    }

    let loc = state.loc();
    let (expr, state) = parse_simple_touple_arg(state)?;
    let mut state = take(",", &state)?;

    let mut expressions = vec![expr];

    let loc = loc.expect("This token must exist at this point");

    loop {
        let (expr, next_state) = fail_if_empty(parse_simple_touple_arg(&state), || {
            loc.error("Expected expression after comma")
        })?;
        expressions.push(expr);
        state = next_state;
        if let Some(next_state) = allow_empty(take(",", &state))? {
            state = next_state;
        } else {
            break;
        }
    }

    Ok((Expression::Touple(expressions).with_loc(&loc), state))
}

fn take_str(state: &State) -> Result<(String, State, Loc), ParseError> {
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

    Ok((value, state, token.loc.clone()))
}

fn parse_str(state: &State) -> ExprRes {
    let (string, state, loc) = take_str(state)?;

    Ok((loc.wrap(Expression::String(string)), state))
}

fn take_variable(state: &State) -> ParseResult<Variable> {
    let (token, state) = state.take_first().map_err(|()| None)?;

    if is_ident(&token) {
        Ok((token.var(), state))
    } else {
        Err(None)
    }
}

fn is_numeric(string: &str) -> bool {
    for c in string.chars() {
        if !c.is_numeric() {
            return false;
        }
    }

    return true;
}

fn is_ident(token: &Token) -> bool {
    if is_numeric(&token.value) {
        return false;
    }
    if let Some(first_char) = token.value.chars().collect::<Vec<char>>().get(0) {
        if *first_char == '\"' {
            return false;
        }
    }
    match token.value.as_ref() {
        "\n" | "(" | ")" | "{" | "}" | "[" | "]" | "," | ".." | "=" | "." => false,
        _ => true,
    }
}

fn parse_func_call(state: &State) -> ExprRes {
    let (target, state) = parse_func_call_target(state)?;
    let (arg, state) = parse_func_arg_expr(&state)?;
    let loc = target.loc.clone();

    Ok((
        loc.wrap(Expression::FuncCall(FuncCall {
            target: Box::new(target),
            arg: Box::new(arg),
            this: Box::new(loc.wrap(Expression::Null)),
        })),
        state,
    ))
}

fn parse_int(state: &State) -> ExprRes {
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

    Ok((
        LocExpr {
            loc: token.loc.clone(),
            expr: Expression::Int(sum),
        },
        state,
    ))
}

fn take_access(state: &State) -> ParseResult<Access> {
    let (target, state) = parse_variable(state)?;

    if !state.next_is(".") {
        return empty();
    }

    let (dot, state) = pick(".", &state)?;

    let (key, state) = take_variable(&state)?;

    let mut access = Access {
        target: Box::new(target.clone()),
        key: key.ident,
        loc: dot.loc.clone(),
    };

    let mut state = state;

    while state.next_is(".") {
        let (dot, next_state) = pick(".", &state)?;
        state = next_state;

        let (key, next_state) = fail_if_empty(take_variable(&state), || {
            dot.loc.error("Required identifier after '.'")
        })?;
        state = next_state;

        access = Access {
            target: Box::new(access.loc.wrap(Expression::Access(access.clone()))),
            loc: dot.loc.clone(),
            key: key.ident,
        };
    }

    Ok((access, state))

    /*
    if state.next_is(".") {
        Ok((
            Access {
                loc: dot_token.loc.clone(),
                target: Box::new(first),
                key: second.value,
            },
            state,
        ))

    } else {
        Ok((
            Access {
                loc: dot_token.loc.clone(),
                target: Box::new(first),
                key: second.value,
            },
            state,
        ))
    }
    */
}

fn parse_access(state: &State) -> ExprRes {
    let (access, state) = take_access(state)?;

    Ok((
        LocExpr {
            loc: access.loc.clone(),
            expr: Expression::Access(access),
        },
        state,
    ))
}

fn parse_import_expr(state: &State) -> ExprRes {
    let (from_token, state) = pick("from", state)?;
    let (source, state, loc) = take_str(&state)?;
    let state = require("import", &state, "Invalid")?;
    if let Some((symbols, state)) = allow_empty(take_comma_separated_variables(&state))? {
        Ok((
            LocExpr {
                loc: from_token.loc.clone(),
                expr: Expression::ImportExpr(ImportExpr {
                    loc,
                    path: source,
                    symbols,
                }),
            },
            state,
        ))
    } else {
        Err(Some(
            from_token
                .loc
                .error("Invalid import. Specify symbols to import."),
        ))
    }
}

fn parse_next_expr_allow_newline(state: &State) -> ExprRes {
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

fn parse_variable(state: &State) -> ExprRes {
    let (var, state) = take_variable(state)?;
    return Ok((
        LocExpr {
            loc: var.loc.clone(),
            expr: Expression::Variable(var),
        },
        state,
    ));
}

fn parse_list(state: &State) -> ExprRes {
    let (opening_bracket, state) = pick("[", state)?;
    let (items, state) = parse_comma_expr_list(state)?;
    let state = require("]", &state, "Unterminated list")?;

    let expr = Expression::List(List {
        items: Rc::new(RefCell::new(items)),
        loc: opening_bracket.loc.clone(),
    });

    return Ok((
        LocExpr {
            expr,
            loc: opening_bracket.loc.clone(),
        },
        state,
    ));
}

fn parse_return_expr(state: &State) -> ExprRes {
    let state = skip_all("\n", state.clone());
    let (ret_token, state) = match allow_empty(pick("return", &state))? {
        Some(value) => value,
        None => pick("ret", &state)?,
    };
    let state = skip_all("\n", state.clone());
    let (expr, state) = if let Some(value) = allow_empty(parse_next_expr(&state))? {
        value
    } else {
        (
            LocExpr {
                loc: ret_token.loc.clone(),
                expr: Expression::Null,
            },
            state,
        )
    };
    Ok((
        LocExpr {
            expr: Expression::Return(Box::new(expr)),
            loc: ret_token.loc.clone(),
        },
        state,
    ))
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

fn take_comma_separated_variables(state: &State) -> ParseResult<Vec<Variable>> {
    let mut vars = Vec::<Variable>::new();
    let mut state = state.clone();

    loop {
        state = match take_variable(&state) {
            Ok((var, state)) => {
                vars.push(var);
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

    let state = skip_all("\n", state.clone());

    return Ok((vars, state));
}

fn parse_func_args(state: &State, required: Optionality) -> ParseResult<(Vec<String>, Loc)> {
    let (token, state) = pick("(", state)?;
    let (tokens, state) = take_comma_separated_variables(&state)?;
    let values = tokens.iter().map(|t| t.ident.clone()).collect();
    let state = if required == Required {
        require(")", &state, "Missing closing parenthesis for function args")?
    } else {
        take(")", &state)?
    };

    Ok(((values, token.loc.clone()), state))
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

fn parse_func_expr_in_func_call_context(state: &State) -> ExprRes {
    // (ident+)
    let (args, args_loc, state) =
        if let Some(((args, loc), state)) = allow_empty(parse_func_args(state, Optional))? {
            (args, Some(loc), state)
        } else {
            (vec![], None, state.clone())
        };

    // {
    let (opening_brace, state) = pick("{", &state)?;

    let loc = match args_loc {
        Some(loc) => loc,
        None => opening_brace.loc,
    };

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
        LocExpr {
            loc,
            expr: Expression::FuncExpr(FuncExpr {
                args,
                expressions: expressions.clone(),
            }),
        },
        state,
    ))
}

fn parse_func_expr(state: &State) -> ExprRes {
    let state = skip_all("\n", state.clone());

    let (args, func_args_loc, state) = match allow_empty(parse_func_args(&state, Required))? {
        Some(((args, loc), state)) => (args, Some(loc), state),
        None => (vec![], None, state),
    };

    let state = skip_all("\n", state);

    let (opening_brace, state) = pick("{", &state)?;

    let loc = match func_args_loc {
        Some(loc) => loc,
        None => opening_brace.loc,
    };

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
        LocExpr {
            loc,
            expr: Expression::FuncExpr(FuncExpr {
                args,
                expressions: expressions.clone(),
            }),
        },
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

        assert_eq!(string.expr, Expression::String(String::from("Hello world")));
        assert_eq!(state.tokens.len(), 0);
    }

    #[test]
    pub fn it_can_parse_a_string_from_parse_next_expr() {
        let (expression, state) = parse_next_expr(&State::new(vec![Token::from(
            "\"Hello world\"",
            Loc::new(1, 1),
        )]))
        .unwrap();

        assert_eq!(
            expression.expr,
            Expression::String(String::from("Hello world"))
        );
        assert_eq!(state.tokens.len(), 0);
    }

    #[test]
    pub fn it_can_parse_an_int_expression() {
        let (expression, _state) =
            parse_next_expr(&State::new(vec![Token::from("123", Loc::new(1, 1))])).unwrap();

        assert_eq!(expression.expr, Expression::Int(123));
    }

    #[test]
    pub fn it_can_parse_a_unary_func_call() {
        let foo_token = Token::from("foo", Loc::new(1, 1));
        let arg_token = Token::from("123", Loc::new(1, 5));

        let (expression, state) =
            parse_next_expr(&State::new(vec![foo_token.clone(), arg_token.clone()])).unwrap();

        assert_eq!(
            expression.expr,
            Expression::FuncCall(FuncCall {
                target: Box::new(Expression::Variable(foo_token.var()).with_loc(&foo_token.loc)),
                arg: Box::new(Expression::Int(123).with_loc(&arg_token.loc)),
                this: Box::new(foo_token.loc.wrap(Expression::Null)),
            })
        );

        assert_eq!(state, State::with_final_loc(1, 5),);
    }

    #[test]
    pub fn it_can_parse_a_program_with_a_func_call() {
        let tokens = lex("print \"Hello world\"");
        let [print, hello_world] = tokens.as_slice() else {
            todo!()
        };
        let ast = parse(&tokens).unwrap();

        let func_call = Expression::FuncCall(FuncCall {
            target: Box::new(print.loc.wrap(Expression::Variable(print.var()))),
            arg: Box::new(
                hello_world
                    .loc
                    .wrap(Expression::String(String::from("Hello world"))),
            ),
            this: Box::new(print.loc.wrap(Expression::Null)),
        });

        assert_eq!(ast, Program::new(vec![func_call.with_loc(&print.loc)]));
    }

    #[test]
    pub fn it_can_parse_a_program_with_multiple_func_calls() {
        let tokens = lex("print \"Hello world\"\nprint \"Foo bar\"");
        let [print1, hello_world, _newline, print2, foo_bar] = tokens.as_slice() else {
            todo!()
        };

        assert_eq!(tokens[0], Token::from("print", Loc::new(1, 1)));

        let ast = parse(&tokens).unwrap();

        assert_eq!(
            ast,
            Program::new(vec![
                print1.loc.func_call(FuncCall {
                    target: print1.loc.wrap(Expression::Variable(print1.var())).boxed(),
                    arg: hello_world
                        .loc
                        .wrap(Expression::String(String::from("Hello world")))
                        .boxed(),
                    this: print1.loc.wrap(Expression::Null).boxed(),
                }),
                print2.loc.func_call(FuncCall {
                    target: print2.var().expr().boxed(),
                    arg: foo_bar.loc.str("Foo bar").boxed(),
                    this: print2.loc.null().boxed(),
                }),
            ],)
        );
    }

    #[test]
    pub fn funky_same_line_multiple_function_calls() {
        let tokens = lex("print \"Hello world\" print \"Foo bar\"");
        let [print1, hello_world, print2, foo_bar] = tokens.as_slice() else {
            panic!()
        };

        assert_eq!(tokens[0], Token::from("print", Loc::new(1, 1)));

        let ast = parse(&tokens).unwrap();

        assert_eq!(
            ast,
            Program::new(vec![
                print1.loc.func_call(FuncCall {
                    target: print1.var().expr().boxed(),
                    arg: hello_world.loc.str("Hello world").boxed(),
                    this: print1.loc.null().boxed(),
                }),
                print2.loc.func_call(FuncCall {
                    target: print2.var().expr().boxed(),
                    arg: foo_bar.loc.str("Foo bar").boxed(),
                    this: print2.loc.null().boxed(),
                }),
            ],)
        );
    }

    #[test]
    pub fn it_can_parse_a_binary_func_call() {
        let tokens = lex("print(\"Hello\", \"world\")");
        let [print, lparen, hello, _comma, world, _rparen] = tokens.as_slice() else {
            panic!()
        };
        let (func_call, _state) = parse_func_call(&State::new(tokens.clone())).unwrap();

        assert_eq!(
            func_call,
            print.loc.func_call(FuncCall {
                target: print.var().expr().boxed(),
                arg: lparen
                    .loc
                    .touple(vec![hello.loc.str("Hello"), world.loc.str("world"),])
                    .boxed(),
                this: print.loc.null().boxed(),
            })
        );
    }

    #[test]
    pub fn it_can_parse_a_touple() {
        let tokens = lex("()");
        let [left, _right] = tokens.as_slice() else {
            panic!()
        };
        let (touple, _state) = parse_touple(&State::new(tokens.clone())).unwrap();

        assert_eq!(touple, left.loc.touple(vec![]));
    }

    #[test]
    pub fn it_can_parse_a_touple_with_expressions() {
        let tokens = lex("(\"foo\", \"bar\")");
        let [lparen, foo, _comma, bar, _rparen] = tokens.as_slice() else {
            panic!()
        };
        let (touple, state) = parse_touple(&State::from(&tokens)).unwrap();

        assert_eq!(
            touple,
            lparen
                .loc
                .touple(vec![foo.loc.str("foo"), bar.loc.str("bar"),])
        );

        assert_eq!(state.tokens.len(), 0);
    }

    #[test]
    pub fn it_can_parse_assignment() {
        let tokens = lex("let x = 10");
        let [let_kw, _x, _eq, ten] = tokens.as_slice() else {
            panic!()
        };
        let (assignment, _state) = parse_var_decl(&State::from(&tokens)).unwrap();

        assert_eq!(
            assignment,
            let_kw.loc.var_decl(VarDecl {
                ident: String::from("x"),
                expr: ten.loc.int(10),
            }),
        );
    }

    #[test]
    pub fn it_can_call_print_with_a_variable() {
        let tokens = lex("print x");
        let [print, x] = tokens.as_slice() else {
            panic!()
        };
        let (func_call, _state) = parse_func_call(&State::new(tokens.clone())).unwrap();

        assert_eq!(
            func_call,
            print.loc.func_call(FuncCall {
                target: print.var().expr().boxed(),
                arg: x.var().expr().boxed(),
                this: print.loc.null().boxed(),
            })
        )
    }

    #[test]
    pub fn you_can_parse_deeply_nested_properties() {
        // This should turn into ((foo).bar).baz";

        let tokens = lex("foo.bar.baz");
        let [foo, d1, _bar, d2, _baz] = tokens.as_slice() else {
            panic!()
        };
        let ast = parse(&tokens).unwrap();

        assert_eq!(
            ast,
            Program {
                expressions: vec![d2.loc.wrap(Expression::Access(Access {
                    loc: d2.loc.clone(),
                    target: d1
                        .loc
                        .wrap(Expression::Access(Access {
                            loc: d1.loc.clone(),
                            target: foo.var().expr().boxed(),
                            key: "bar".to_string(),
                        }))
                        .boxed(),
                    key: "baz".to_string(),
                }))],
            }
        );
    }
}

/*
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

        let tokens = lex(source);

        assert_eq!(
            parse(&tokens).unwrap(),
            Program::new(vec![Expression::ReAssignment(Box::new(ReAssignment {
                ident: Identifier {
                    accessors: vec![tokens[0].clone()]
                },
                expr: Expression::Int(1),
            }))])
        );
    }

    #[test]
    pub fn you_can_reassign_variables() {
        let source = r#"
            let x = 1
            x = 2
            print x
        "#;

        let tokens = lex(source);

        assert_eq!(
            parse(&tokens).unwrap(),
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
                    ident: Identifier {
                        accessors: vec![Token {
                            value: "x".to_string(),
                            loc: Loc::new(3, 13),
                        }]
                    },
                    expr: Expression::Int(2),
                })),
                Expression::FuncCall(FuncCall {
                    ident: Identifier {
                        accessors: vec![Token {
                            value: "print".to_string(),
                            loc: Loc::new(4, 13),
                        }]
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "x",
                        Loc::new(4, 19)
                    )))),
                    this: Box::new(Expression::Null),
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

        assert_eq!(state, State::with_final_loc(2, 15),);
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
                    ident: Identifier {
                        accessors: vec![Token {
                            value: String::from("print"),
                            loc: Loc::new(3, 17)
                        }]
                    },
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                    this: Box::new(Expression::Null),
                })],
            })
        );

        assert_eq!(state, State::with_final_loc(4, 14),);
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
                    ident: Identifier {
                        accessors: vec![Token {
                            value: String::from("print"),
                            loc: Loc::new(2, 22)
                        }]
                    },
                    arg: Box::new(Expression::String(String::from("Hello world"))),
                    this: Box::new(Expression::Null),
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
                    ident: Identifier {
                        accessors: vec![Token {
                            value: "print".to_string(),
                            loc: Loc::new(2, 13),
                        }]
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "a",
                        Loc::new(2, 19),
                    )))),
                    this: Box::new(Expression::Null),
                }),
                Expression::FuncCall(FuncCall {
                    ident: Identifier {
                        accessors: vec![Token {
                            value: String::from("print"),
                            loc: Loc::new(3, 13),
                        }]
                    },
                    arg: Box::new(Expression::Variable(Variable::new(Token::from(
                        "b",
                        Loc::new(3, 19),
                    )))),
                    this: Box::new(Expression::Null),
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

        let tokens = lex(source);
        let program = parse(&tokens).unwrap();

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
                        ident: Identifier {
                            accessors: vec![Token {
                                value: "print".to_string(),
                                loc: Loc::new(3, 17),
                            }]
                        },
                        arg: Box::new(Expression::String(String::from("Hello world"))),
                        this: Box::new(Expression::Null),
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
                range: Box::new(RangeLiteral {
                    start: Expression::Int(0),
                    end: Expression::Int(10),
                }),
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
                range: Box::new(RangeLiteral {
                    start: Expression::Int(0),
                    end: Expression::Int(10),
                }),
                body: vec![Expression::Int(123),],
            })])
        );
    }

    #[test]
    pub fn it_can_parse_a_while_loop() {
        let source = r#"while lt(i, 10) {123}"#;

        let tokens = lex(source);
        let program = parse(&tokens).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::WhileExpr(WhileExpr {
                condition: Box::new(Expression::FuncCall(FuncCall {
                    target: Box::new(Expression::Variable(Variable::new(tokens[1].clone()))),
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
                    this: Box::new(Expression::Null),
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
                    range: Box::new(RangeLiteral {
                        start: Expression::Int(0),
                        end: Expression::Int(10),
                    }),
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
            Program::new(vec![Expression::List(List {
                loc: Loc { line: 1, column: 1 },
                items: Rc::new(RefCell::new(vec![
                    Expression::Int(5),
                    Expression::Int(7),
                    Expression::Int(13),
                    Expression::Int(29),
                ])),
            })])
        );
    }

    #[test]
    fn it_can_parse_function_args() {
        let source = "(a, b, c) {\n}";

        let ast = parse(&lex(source)).unwrap();

        assert_eq!(
            ast,
            Program::new(vec![Expression::FuncExpr(FuncExpr {
                args: vec!["a".to_string(), "b".to_string(), "c".to_string()],
                expressions: vec![],
            })])
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
            Program::new(vec![Expression::VarDecl(Box::new(VarDecl {
                ident: "x".to_string(),
                loc: Loc::new(2, 13),
                expr: Expression::FuncExpr(FuncExpr {
                    args: vec![],
                    expressions: vec![Expression::Return(Box::new(Expression::Int(1))),],
                })
            }))]),
        );
    }

    #[test]
    fn nested_func_calls() {
        let source = r#"eq(1, x())"#;
        let tokens = lex(source);

        let program = parse(&tokens).unwrap();

        assert_eq!(
            program,
            Program::new(vec![Expression::FuncCall(FuncCall {
                ident: Identifier {
                    accessors: vec![tokens[0].clone()]
                },
                arg: Box::new(Expression::Touple(vec![
                    Expression::Int(1),
                    Expression::FuncCall(FuncCall {
                        ident: Identifier {
                            accessors: vec![tokens[4].clone()]
                        },
                        arg: Box::new(Expression::Touple(vec![])),
                        this: Box::new(Expression::Null),
                    }),
                ])),
                this: Box::new(Expression::Null),
            })])
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
                Program::new(vec![Expression::Access(Access {
                    loc: Loc::new(1, 4),
                    target: Box::new(Expression::Variable(Variable {
                        loc: Loc::new(1, 1),
                        ident: "foo".to_string(),
                    })),
                    key: "bar".to_string(),
                }),],),
            );
        }

        #[test]
        fn assignment() {
            let source = r#"foo.bar = 123"#;
            let tokens = lex(source);

            let ast = parse(&tokens).unwrap();

            assert_eq!(
                ast,
                Program::new(vec![Expression::ReAssignment(Box::new(ReAssignment {
                    ident: Identifier {
                        accessors: vec![tokens[0].clone(), tokens[2].clone()]
                    },
                    expr: Expression::Int(123),
                }))],),
            );
        }

        #[test]
        fn method_call() {
            let source = r#"foo.bar()"#;
            let tokens = lex(source);

            let ast = parse(&tokens).unwrap();

            assert_eq!(
                ast,
                Program::new(vec![Expression::FuncCall(FuncCall {
                    ident: Identifier {
                        accessors: vec![
                            Token {
                                value: "foo".to_string(),
                                loc: Loc::new(1, 1),
                            },
                            Token {
                                value: "bar".to_string(),
                                loc: Loc::new(1, 5),
                            },
                        ]
                    },
                    arg: Box::new(Expression::Touple(vec![])),
                    this: Box::new(Expression::Null),
                })]),
            );
        }
    }

    mod modules {
        use super::*;
        use pretty_assertions_sorted::assert_eq;

        #[test]
        fn it_can_parse_imports() {
            let source = r#"from "./foo.fun" import bar"#;
            let tokens = lex(source);
            let program = parse(&tokens).unwrap();

            assert_eq!(
                program,
                Program::new(vec![Expression::ImportExpr(ImportExpr {
                    loc: Loc::new(1, 6),
                    path: "./foo.fun".to_string(),
                    symbols: vec![Token {
                        value: "bar".to_string(),
                        loc: Loc::new(1, r#"from "./foo.fun" import "#.len() + 1),
                    }],
                })])
            );
        }
    }
}
*/
