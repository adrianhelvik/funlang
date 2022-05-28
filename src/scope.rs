use crate::types::*;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Scope<'a> {
    pub values: RefCell<HashMap<&'a str, Expression>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn testing() {
        let scope = Scope {
            values: { RefCell::new(HashMap::new()) },
        };

        let mut values = scope.values.borrow_mut();

        values.insert("message", Expression::String(String::from("Hello world")));

        let actual = values.get("message").unwrap();
        let expected = Expression::String(String::from("Hello world"));

        assert_eq!(actual, &expected);
    }

    /*
    #[test]
    pub fn it_can_store_values() {
        let scope = Scope::new();

        scope.set("message", Expression::String(String::from("Hello world")));

        assert_eq!(
            scope.get("message").unwrap(),
            &Expression::String(String::from("Hello world")),
        );
    }

    #[test]
    pub fn it_can_be_extended() {
        let scope = Scope::new();

        scope.set(String::from("a"), Expression::String(String::from("A")));

        scope.set(String::from("b"), Expression::String(String::from("B")));

        let child_scope = scope.extend();

        child_scope.set("b", Expression::String(String::from("overridden")));

        assert_eq!(
            *scope.get("a").unwrap(),
            Expression::String(String::from("A")),
        );

        assert_eq!(
            *scope.get("b").unwrap(),
            Expression::String(String::from("overridden")),
        );
    }
    */
}
