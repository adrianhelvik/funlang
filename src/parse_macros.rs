#[macro_export]
macro_rules! parse_one_of {
    ($state:expr, parsers: [ $($func:ident),+ $(,)? ] $(,)?) => {
        let state = $state;

        if state.len() == 0 {
            return Err(None);
        };

        $(
            if let Some((value, state)) = allow_empty($func(&state))? {
                return Ok((value, state));
            }
        )+
    };
    ($state:expr, transform_state_after: $transform_state_after:expr, parsers: [ $($func:ident),+ $(,)? ] $(,)?) => {
        let state = $state;

        if state.len() == 0 {
            return Err(None);
        };

        $(
            if let Some((value, state)) = allow_empty($func(&state))? {
                let state = $transform_state_after(state);
                return Ok((value, state));
            }
        )+
    };
}
