pub mod builtins;
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
//use std::time::Instant;
use types::*;

pub fn eval(source: &str) -> Result<(), SyntaxError> {
    //let start = Instant::now();
    let tokens = lex(source);
    //println!("lexed in {} seconds", start.elapsed().as_secs_f32());

    //let start = Instant::now();
    match parse(tokens) {
        Ok(program) => {
            //println!("parsed in {} seconds", start.elapsed().as_secs_f32());
            let output = Rc::new(RefCell::new(std::io::stdout()));
            //let start = Instant::now();
            match interpreter::eval_program(&program, &output) {
                Ok(_) => {
                    //println!("evaluated in {} seconds", start.elapsed().as_secs_f32());
                    Ok(())
                }
                Err(err) => Err(SyntaxError::generate(err, source.to_string())),
            }
        }
        Err(err) => Err(SyntaxError::generate(err, source.to_string())),
    }
}

pub fn test_eval(source: &str) -> String {
    let output = Rc::new(RefCell::new(Vec::new()));
    let tokens = lex(source);
    match parse(tokens) {
        Ok(program) => {
            match interpreter::eval_program(&program, &output) {
                Ok(_) => {}
                Err(err) => {
                    return SyntaxError::generate_plain(err, source.to_string()).message;
                }
            }
            let output = output.borrow();
            String::from_utf8(output.to_vec()).unwrap()
        }
        Err(err) => err.message,
    }
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

    #[test]
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

    #[test]
    pub fn you_can_forward_multiple_func_args() {
        let source = r#"
            let my_add = (a, b) {
                return add(a, b)
            }
            print my_add(1, 2)
        "#;

        assert_eq!(test_eval(source), "3");
    }

    #[test]
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
    fn it_can_create_a_function_in_a_function() {
        let source = r#"
            let outer = (thing) {
                let inner = {
                    print thing
                }
                inner()
            }
        "#;

        assert_eq!(test_eval(source), "");
    }

    #[test]
    fn it_can_create_a_function_in_a_function_and_call_it() {
        let source = r#"
            let outer = (thing) {
                let inner = {
                    print thing
                }
                inner()
            }
            outer "Hello world"
        "#;

        assert_eq!(test_eval(source), "Hello world");
    }

    #[test]
    pub fn it_can_preserve_enclosed_values() {
        let source = r#"
            let printer = (thing) {
                return {
                    println thing
                }
            }
            println "Starting"
            let p = printer("Hello world")
            println type(p)
            p()
            println "Done"
        "#;

        assert_eq!(test_eval(source), "Starting\nclosure\nHello world\nDone\n");
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
    pub fn if_is_truthy_for_one_and_zero() {
        let source = r#"
            if 1 {
                println "first"
            }
            if 0 {
                println "second"
            }
        "#;

        assert_eq!(test_eval(source), "first\nsecond\n");
    }

    #[test]
    pub fn less_than_can_be_true() {
        let source = r#"
            print lt(1, 2)
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn less_than_can_be_false() {
        let source = r#"
            print lt(2, 2)
        "#;

        assert_eq!(test_eval(source), "false");
    }

    #[test]
    pub fn less_than_with_variables() {
        let source = r#"
            let first = 100
            let second = 1
            print lt(first, second)
        "#;

        assert_eq!(test_eval(source), "false");
    }

    #[test]
    pub fn less_than_with_variables2() {
        let source = r#"
            let first = 1
            let second = 100
            print lt(first, second)
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

        assert_eq!(test_eval(source), "1\n");
    }

    #[test]
    pub fn it_will_only_use_the_first_return() {
        let source = "
            let func = () {
                ret 1
                ret 2
            }
            print func()
        ";

        assert_eq!(test_eval(source), "1");
    }

    #[test]
    pub fn it_can_return_in_an_if_statement() {
        let source = r#"
            let func = () {
                println 1
                if eq(1, 1) {
                    println 2
                    ret "inner"
                    println 3
                }
                println 4
                ret "outer"
                println 5
            }
            print func()
        "#;

        assert_eq!(test_eval(source), "1\n2\ninner");
    }

    #[test]
    pub fn exploratory_recursion2() {
        let source = r#"
            let sum_below = (n) {
                if lte(n, 0) {
                    ret 0
                }

                ret add(n, sum_below(sub(n, 1)))
            }
            println sum_below 3
        "#;

        assert_eq!(test_eval(source), "6\n");
    }

    #[test]
    pub fn it_can_create_a_map() {
        let source = r#"
            let my_map = Map()

            my_map("message", "Hello world!")

            let value = my_map("message")

            print value
        "#;

        assert_eq!(test_eval(source), "Hello world!");
    }

    mod eq {
        use super::*;

        #[test]
        pub fn eq_equal_variables() {
            let source = r#"
                let a = 123
                let b = 123

                print eq(a, b)
            "#;

            assert_eq!(test_eval(source), "true");
        }

        #[test]
        pub fn eq_unequal_variables() {
            let source = r#"
                let a = 111
                let b = 222

                print eq(a, b)
            "#;

            assert_eq!(test_eval(source), "false");
        }

        #[test]
        fn eq_differing_variable_types() {
            let source = r#"
                let a = 111
                let b = "111"

                print eq(a, b)
            "#;

            assert_eq!(test_eval(source), "false");
        }

        #[test]
        fn eq_variable_strings() {
            let source = r#"
                let a = "Hey"
                let b = "Hey"

                print eq(a, b)
            "#;

            assert_eq!(test_eval(source), "true");
        }

        #[test]
        fn variable_and_int() {
            let source = r#"
                let x = 1

                print eq(x, 1)
            "#;

            assert_eq!(test_eval(source), "true");
        }

        #[test]
        fn int_and_variable() {
            let source = r#"
                let x = 1

                print eq(1, x)
            "#;

            assert_eq!(test_eval(source), "true");
        }

        #[test]
        fn int_and_func_call() {
            let source = r#"
                let x = () { ret 1 }

                print eq(1, x())
            "#;

            assert_eq!(test_eval(source), "true");
        }
    }

    mod modulo {
        use super::*;

        #[test]
        pub fn numbers() {
            let source = r#"
                let a = 123
                let b = 123

                print modulo(a, b)
            "#;

            assert_eq!(test_eval(source), "0");
        }

        #[test]
        pub fn one_above() {
            let source = r#"
                let a = 123
                let b = 122

                print modulo(a, b)
            "#;

            assert_eq!(test_eval(source), "1");
        }

        #[test]
        pub fn overflowing() {
            let source = r#"
                let a = 27
                let b = 5

                print modulo(a, b)
            "#;

            assert_eq!(test_eval(source), "2");
        }
    }

    #[test]
    pub fn or_true_true() {
        let source = r#"
            let a = true
            let b = true

            print or(a, b)
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn or_true_false() {
        let source = r#"
            let a = true
            let b = false

            print or(a, b)
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn or_false_true() {
        let source = r#"
            let a = false
            let b = true

            print or(a, b)
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn or_false_false() {
        let source = r#"
            let a = false
            let b = false

            print or(a, b)
        "#;

        assert_eq!(test_eval(source), "false");
    }

    #[test]
    pub fn and_true_true() {
        let source = r#"
            let a = true
            let b = true

            print and(a, b)
        "#;

        assert_eq!(test_eval(source), "true");
    }

    #[test]
    pub fn boolean_and_true_false() {
        let source = r#"
            let a = true
            let b = false

            print and(a, b)
        "#;

        assert_eq!(test_eval(source), "false");
    }

    #[test]
    pub fn boolean_and_false_false() {
        let source = r#"
            let a = false
            let b = false

            print and(a, b)
        "#;

        assert_eq!(test_eval(source), "false");
    }

    mod not {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        pub fn not_touple_true() {
            let source = r#"
                print not (true)
            "#;

            assert_eq!(test_eval(source), "false");
        }

        #[test]
        pub fn not_true() {
            let source = r#"
                print not true
            "#;

            assert_eq!(test_eval(source), "false");
        }

        #[test]
        pub fn not_touple_false() {
            let source = r#"
                print not (false)
            "#;

            assert_eq!(test_eval(source), "true");
        }

        #[test]
        pub fn not_false() {
            let source = r#"
                print not false
            "#;

            assert_eq!(test_eval(source), "true");
        }
    }

    mod error_messages {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn incorrect_not_equals() {
            let source = String::from("if 1 != 2 { println\"Hello world\" }");

            let mut expected = String::new();
            expected.push_str("Error: if 1 != 2 { println\"Hello world\" }\n");
            expected.push_str("            ^ [1:6] Attempted to assign undeclared variable '!'");

            let actual = test_eval(&source);

            if actual == expected {
                return;
            }

            eprintln!("EXPECTED:");
            eprintln!("{}", expected);
            eprintln!("");
            eprintln!("ACTUAL:");
            eprintln!("{}", actual);
            eprintln!("");

            panic!("Not equal");
        }
    }

    mod type_builting {
        use crate::test_eval;
        use pretty_assertions::assert_eq;

        #[test]
        fn it_can_get_the_type_of_a_string() {
            let source = r#"
                print type "Hello world"
            "#;

            assert_eq!(test_eval(source), "string");
        }

        #[test]
        fn it_can_get_the_type_of_a_function_expression() {
            let source = r#"
                print type () {}
            "#;

            assert_eq!(test_eval(source), "closure");
        }

        #[test]
        fn it_can_get_the_type_of_an_integer() {
            let source = r#"
                print type 123
            "#;

            assert_eq!(test_eval(source), "integer");
        }
    }

    mod loops {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn it_can_do_for_in_range() {
            let source = r#"
                for i in 0..10 {
                    println i
                }
            "#;

            assert_eq!(test_eval(source), "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n");
        }

        #[test]
        fn it_can_do_while() {
            let source = r#"
                let i = 0
                while lt(i, 10) {
                    println i
                    i = add(i, 1)
                }
            "#;

            assert_eq!(test_eval(source), "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n");
        }
    }

    //#[test]
    fn it_optimizes_tail_calls() {
        let source = r#"
            let i = 0
            let loop = () {
                if lt(i, 1000) {
                    i = add(i, 1)
                    loop()
                }
            }
            loop()
            println("i = ", i)
            print "Done!"
        "#;

        assert_eq!(test_eval(source), "i = 1000\nDone!");
    }
}
