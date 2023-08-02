pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod types;

use lexer::lex;
use parser::parse;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::rc::Rc;
use types::*;

pub fn eval(source: &str) -> Result<(), SyntaxError> {
    // let stdout: RefCell<Box<dyn Write>> = RefCell::new(Box::new(std::io::stdout()));
    // let stderr: RefCell<Box<dyn Write>> = RefCell::new(Box::new(std::io::stderr()));

    let tokens = lex(source);
    match parse(tokens) {
        Ok(program) => {
            let output = Rc::new(RefCell::new(std::io::stdout()));
            interpreter::eval_program(program, Rc::clone(&output));
            Ok(())
        }
        Err(err) => Err(SyntaxError::generate(err, source.to_string())),
    }
}

pub fn test_eval(source: &str) -> String {
    let output = Rc::new(RefCell::new(Vec::new()));
    let tokens = lex(source);
    let program = parse(tokens).unwrap();
    interpreter::eval_program(program, Rc::clone(&output));
    let output = output.borrow();
    String::from_utf8(output.to_vec()).unwrap()
}

pub fn eval_or_panic(source: &str) {
    if let Err(err) = eval(source) {
        eprintln!("{}", err.message);
        std::process::exit(1);
    }
}

pub fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("You must supply a file type. Args: {:?}", args);
    }
    let filename = args[1].clone();

    let contents = fs::read_to_string(filename).expect("Failed to read file");

    eval_or_panic(&contents);
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use types::Program;

    #[allow(dead_code)]
    pub fn lax(source: &str) -> Program {
        parse(lex(source)).unwrap()
    }

    #[test]
    pub fn you_can_print_something() {
        let source = r#"
            print "Hello world"
        "#;

        assert_eq!(test_eval(source), "Hello world");
    }

    #[test]
    pub fn you_can_add_numbers() {
        let source = r#"
            print add(105, 20)
        "#;

        assert_eq!(test_eval(source), "125");
    }

    #[test]
    pub fn you_can_assign_variables() {
        let source = r#"
            let x = 10
            print x
        "#;

        assert_eq!(test_eval(source), "10");
    }

    #[test]
    pub fn blocks_are_lazily_evaluated() {
        let source = r#"
            {
                println "Hello world"
            }
        "#;

        assert_eq!(test_eval(source), "");
    }

    #[test]
    pub fn you_can_call_functions_without_args() {
        let source = r#"
            let block = {
                println "Hello world"
            }
            block()
        "#;
        assert_eq!(test_eval(source), "Hello world\n");
    }

    #[test]
    pub fn you_can_create_child_scopes() {
        let source = r#"
            let x = "outer"
            let f = {
                let x = "inner"
                print(x)
                print " "
            }
            f()
            print x
        "#;

        assert_eq!(test_eval(source), "inner outer");
    }

    #[test]
    pub fn you_can_assign_to_parent_scopes() {
        let source = r#"
            let x = "outer"
            let f = {
                x = "inner"
            }
            f()
            print x
        "#;

        assert_eq!(test_eval(source), "inner");
    }

    #[test]
    pub fn you_can_add_variables() {
        let source = r#"
            let x = 1
            let y = 2
            print add(x, y)
        "#;

        assert_eq!(test_eval(source), "3");
    }

    #[test]
    pub fn you_can_nest_add() {
        let source = r#"
            print add(add(1, 2), 3)
        "#;

        assert_eq!(test_eval(source), "6");
    }

    //#[test]
    pub fn you_can_forward_one_func_args() {
        let source = r#"
            let my_add = (x) {
                return add(x)
            }
            print my_add(1)
        "#;

        println!("{:?}", lax(source));

        assert_eq!(test_eval(source), "1");
    }

    //#[test]
    pub fn you_can_forward_multiple_func_args() {
        let source = r#"
            let my_add = (a, b) {
                return add(a, b)
            }
            print my_add(1, 2)
        "#;

        assert_eq!(test_eval(source), "3");
    }

    //#[test]
    pub fn you_can_call_a_func_with_args() {
        assert_eq!(test_eval("print add(1, 2)"), "3");
        let source = r#"
            let random_func = (a, b) {
                return add(add(a, b), 10)
            }
            print random_func(1, 2)
        "#;

        assert_eq!(test_eval(source), "13");
    }

    #[test]
    pub fn you_can_return_an_int_from_a_func() {
        let source = r#"
            let f = { return 1 }
            print f()
        "#;

        assert_eq!(test_eval(source), "1");
    }

    #[test]
    pub fn functions_can_receive_an_argument() {
        let source = r#"
            let f = (x) { print x }
            f 1
        "#;

        assert_eq!(test_eval(source), "1");
    }

    #[test]
    pub fn functions_can_receive_a_touple_argument() {
        let source = r#"
            let f = (a, b) {
                print(a)
                print b
            }
            f(1, 2)
        "#;

        assert_eq!(test_eval(source), "12");
    }

    #[test]
    pub fn it_prints_a_nice_message_when_erroring() {
        let source = "{";
        let error_message = eval(source).unwrap_err().message;

        assert_eq!(error_message, "{\n^ [1:1] Unexpected token '{'");
    }

    #[test]
    pub fn it_can_preserve_enclosed_values() {
        let source = r#"
            let printer = (thing) {
                return {
                    println thing
                }
            }
            let p = printer("Hello world")
            p()
        "#;

        assert_eq!(test_eval(source), "Hello world\n");
    }

    #[test]
    pub fn it_can_preserve_enclosed_values_2() {
        let source = r#"
            let printer = (thing) {
                let print_thing = {
                    println thing
                }
                print_thing()
                return print_thing
            }
            let p = printer("Hello world")
            p()
        "#;

        assert_eq!(test_eval(source), "Hello world\nHello world\n");
    }
}
