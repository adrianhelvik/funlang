pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod types;

use lexer::lex;
use parser::parse;
use std::env;
use std::fs;
use types::Program;

pub fn eval(source: &str) -> String {
    interpreter::eval_program(parse(lex(source)))
}

pub fn lax(source: &str) -> Program {
    parse(lex(source))
}

pub fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        panic!("You must supply a file type");
    }
    let filename = args[2].clone();

    let contents = fs::read_to_string(filename).expect("Failed to read file");

    println!("{}", eval(&contents));
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    pub fn you_can_print_something() {
        let source = r#"
            print "Hello world"
        "#;

        assert_eq!(eval(source), "Hello world");
    }

    #[test]
    pub fn you_can_add_numbers() {
        let source = r#"
            print add(105, 20)
        "#;

        assert_eq!(eval(source), "125");
    }

    #[test]
    pub fn you_can_assign_variables() {
        let source = r#"
            x: 10
            print x
        "#;

        assert_eq!(eval(source), "125");
    }
}
