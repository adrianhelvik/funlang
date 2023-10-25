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
            match interpreter::eval_program(program, Rc::clone(&output)) {
                Ok(_) => Ok(()),
                Err(err) => Err(SyntaxError::generate(err, source.to_string())),
            }
        }
        Err(err) => Err(SyntaxError::generate(err, source.to_string())),
    }
}

pub fn test_eval(source: &str) -> String {
    let output = Rc::new(RefCell::new(Vec::new()));
    let tokens = lex(source);
    let program = parse(tokens).unwrap();
    match interpreter::eval_program(program, Rc::clone(&output)) {
        Ok(_) => {}
        Err(err) => {
            println!(
                "{}",
                SyntaxError::generate(err.clone(), source.to_string()).message
            );
            panic!("{}", err.message);
        }
    }
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

    #[test]
    pub fn if_is_truthy_for_one_and_falsey_for_zero() {
        let source = r#"
            if 1 {
                print "true"
            }
            if 0 {
                print "false"
            }
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn less_than_can_be_true() {
        let source = r#"
            print less_than(1, 2)
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn less_than_can_be_false() {
        let source = r#"
            print less_than(2, 2)
        "#;

        assert_eq!(test_eval(source), "false");
    }

    #[test]
    pub fn less_than_with_variables() {
        let source = r#"
            let first = 100
            let second = 1
            print less_than(first, second)
        "#;

        assert_eq!(test_eval(source), "false");
    }

    #[test]
    pub fn less_than_with_variables2() {
        let source = r#"
            let first = 1
            let second = 100
            print less_than(first, second)
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn you_can_define_a_new_variable_within_func_based_on_another() {
        let source = r#"
            let i = 0
            let func = {
                let j = add(i, 1)
                print j
            }
            func()
        "#;

        assert_eq!(test_eval(source), "1");
    }

    #[test]
    pub fn reassignment_in_func() {
        let source = r#"
            let i = 0
            let func = {
                i = add(i, 1)
                print i
            }
            func()
        "#;

        assert_eq!(test_eval(source), "1");
    }

    #[test]
    pub fn exploratory_recursion() {
        let source = r#"
            let func = (n) {
                ret n
            }
            println func 1
        "#;

        assert_eq!(test_eval(source), "1");
    }

    #[test]
    pub fn exploratory_recursion2() {
        let source = r#"
            let sum_below = (n) {
                ret add(n, sum_below(sub(n, 1)))
            }
            println sum_below 3
        "#;

        assert_eq!(test_eval(source), "6");
    }
}
