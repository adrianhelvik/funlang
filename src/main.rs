pub mod setup_builtins;
pub mod builtins;
pub mod parse_macros;
pub mod context;
pub mod interpreter;
pub mod scope;
pub mod lexer;
pub mod parser;
pub mod types;
mod util;

use lexer::lex;
use parser::parse;
use resolve_path::PathResolveExt;
use std::cell::RefCell;
use std::env;
use std::fs;
use std::process::exit;
use std::rc::Rc;
//use std::time::Instant;
use types::*;

pub fn eval(source: &str, filename: &str) -> Result<(), SyntaxError> {
    //let start = Instant::now();
    let tokens = lex(source);

    //let start = Instant::now();
    match parse(&tokens) {
        Ok(program) => {
            let program = Program {
                expressions: program.expressions.into(),
            };
            //println!("parsed in {} seconds", start.elapsed().as_secs_f32());
            let output = Rc::new(RefCell::new(std::io::stdout()));
            //let start = Instant::now();
            match interpreter::eval_program(&program, &output, filename) {
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
    let curdir = env::current_dir().unwrap();
    let cwd = curdir.to_str().unwrap();
    let root_cow = "./test.fun".resolve_in(cwd);
    let root = root_cow.to_str().unwrap();
    match parse(&tokens) {
        Ok(program) => {
            match interpreter::eval_program(&program, &output, root) {
                Ok(_) => {}
                Err(err) => {
                    return SyntaxError::generate_plain(err, source.to_string()).message;
                }
            }
            let output = output.borrow();
            String::from_utf8(output.to_vec()).unwrap()
        }
        Err(err) => SyntaxError::generate_plain(err, source.to_string()).message,
    }
}

pub fn eval_or_panic(source: &str, filename: &str) {
    if let Err(err) = eval(source, filename) {
        eprintln!("{}", err.message);
        std::process::exit(1);
    }
}

pub fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        help();
    }
    match args.len() {
        2 => {
            let curdir = env::current_dir().unwrap();
            let cwd = curdir.to_str().unwrap();
            let tmp0 = args[1].clone();
            let tmp = tmp0.resolve_in(cwd);
            let filename = tmp.to_str().unwrap();

            let contents = fs::read_to_string(&filename).expect("Failed to read file");

            eval_or_panic(&contents, &filename);
        }
        3 => {
            if args[1] == "-e" {
                eval_or_panic(&args[2], "eval");
            } else {
                help();
            }
        }
        _ => help(),
    }
}

fn help() {
    eprintln!("You must supply a file name or use the -e flag to evaluate code");
    exit(1);
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use types::Program;

    macro_rules! assert_lines_equal {
        ($source:expr, $expected:expr) => {{
            let output = test_eval($source);
            let mut result = output.split("\n").collect::<Vec<_>>();
            while result.last() == Some(&"") {
                result.pop();
            }
            assert_eq!(result, $expected);
        }};
    }

    #[allow(dead_code)]
    pub fn lax(source: &str) -> Program {
        parse(&lex(source)).unwrap()
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

            my_map.message = "Hello world!"

            let value = my_map.message

            print value
        "#;

        assert_eq!(test_eval(source), "Hello world!");
    }

    mod eq {
        use super::*;
        use pretty_assertions::assert_eq;

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
        use pretty_assertions::assert_eq;

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

        #[test]
        fn incorrect_not_equals() {
            let source = String::from("if 1 != 2 { println\"Hello world\" }");

            let mut expected = String::new();
            expected.push_str("Error: if 1 != 2 { println\"Hello world\" }\n");
            expected.push_str("            ^ [1:6] Attempted to reassign undeclared variable '!'");

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

    mod type_builtins {
        use super::*;
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
                print type(() {})
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

        //#[test]
        fn it_can_do_for_in_list() {
            assert_lines_equal!(
                r#"
                    let list = []

                    list.push(2)
                    list.push(3)
                    list.push(5)
                    list.push(7)

                    for item in list {
                        println item
                    }
                "#,
                vec![
                    "2",
                    "3",
                    "5",
                    "7",
                ]
            );
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

    mod lists {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn it_can_create_a_list() {
            let source = r#"
                let my_list = [1, 2, add(1, 2)]
                print my_list
            "#;

            assert_eq!(test_eval(source), "[1, 2, 3]");
        }

        #[test]
        fn it_can_push_to_a_list() {
            let source = r#"
                let my_list = [1, 2, add(1, 2)]
                my_list.push(4)
                my_list.push(5)
                print my_list
            "#;

            assert_eq!(test_eval(source), "[1, 2, 3, 4, 5]");
        }

        #[test]
        fn it_can_get_the_length_of_a_list() {
            let source = r#"
                let my_list = [1, 2, add(1, 2)]
                my_list.push(4)
                my_list.push(5)
                print my_list.len()
            "#;

            assert_eq!(test_eval(source), "5");
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

    #[test]
    fn it_can_import_and_not_do_anything() {
        let source = r#"
            from "./lib/list.fun" import list
        "#;

        assert_eq!(test_eval(source), "");
    }

    #[test]
    fn it_can_import_symbols() {
        let source = r#"
            from "./lib/list.fun" import list

            let my_list = list()
            my_list.insert(123)
            my_list.insert(42)
            my_list.insert(69)
            my_list.insert(1337)

            print my_list.len()
        "#;

        assert_eq!(test_eval(source), "4");
    }

    #[test]
    fn it_can_import_multiple_symbols() {
        let source = r#"
            from "./lib/exporter.fun" import foo, bar
            println foo
            println bar
        "#;

        assert_eq!(test_eval(source), "123\n456\n");
    }

    #[test]
    fn it_can_apply_a_function_to_a_closure() {
        let source = r#"
            let cached = (func) {
                let cache = Map()
                ret (arg) {
                    let value = cache(str(arg))
                    if not eq(value, null) {
                        ret cache(str(arg))
                    }
                    let value = func(arg)
                    cache(str(arg), value)
                    ret value
                }
            }

            let fib = cached (n) {
                if lte(n, 1) {
                    ret 1
                }

                ret add(fib(sub(n, 1)), fib(sub(n, 2)))
            }

            print fib 4
        "#;

        assert_eq!(test_eval(source), "5");
    }

    #[test]
    fn it_can_apply_a_function_to_a_closure2() {
        let source = r#"
            let passthrough = (func) {
                ret (arg) {
                    println("in passthrough cb: ", arg)
                    ret func(arg)
                }
            }

            let foo = passthrough (n) {
                println("in foo: ", n)
                ret n
            }

            print foo 10
        "#;

        assert_eq!(test_eval(source), "in passthrough cb: 10\nin foo: 10\n10");
    }

    #[test]
    fn it_can_apply_a_function_to_a_closure3() {
        assert_lines_equal!(
            r#"
                let cached = (func) {
                    let cache = Map()
                    ret (arg) {
                        let value = cache(str(arg))
                        if not eq(value, null) {
                            ret cache(str(arg))
                        }
                        let value = func(arg)
                        cache(str(arg), value)
                        ret value
                    }
                }

                let fib = cached (n) {
                    println("fib(", n, ")")
                    if lte(n, 1) {
                        ret 1
                    }

                    ret add(fib(sub(n, 1)), fib(sub(n, 2)))
                }

                fib 10
            "#,
            vec![
                "fib(10)", "fib(9)", "fib(8)", "fib(7)", "fib(6)", "fib(5)", "fib(4)", "fib(3)",
                "fib(2)", "fib(1)", "fib(0)",
            ]
        );
    }

    #[test]
    fn func_call_presedence() {
        assert_lines_equal!(
            r#"
                let value = null
                println("is value not null? ", not eq(value, null))
            "#,
            vec!["is value not null? false"]
        );
    }

    #[test]
    fn if_with_contained_function_call() {
        assert_lines_equal!(
            r#"
                if eq(true, true) {
                    print "yes!"
                }
            "#,
            vec!["yes!"]
        );
    }

    #[test]
    fn it_can_escape_block_context_in_touples() {
        assert_lines_equal!(
            r#"
                let call_closure_and_return_true = (inner) {
                    inner()
                    ret true
                }
                if (call_closure_and_return_true () { println "inner" }) {
                    println "inside if block"
                }
            "#,
            vec!["inner", "inside if block",]
        );
    }

    #[test]
    fn return_without_args() {
        assert_lines_equal!(
            r#"
                let foo = () {
                    if true {
                        ret
                    }

                    ret "whoops"
                }

                print foo()
            "#,
            vec!["null"]
        );
    }

    #[test]
    fn you_can_create_a_closure_without_args() {
        assert_lines_equal!(
            r#"
                let my_func = {
                    println "Hello world"
                }

                my_func()
            "#,
            vec!["Hello world"]
        );
    }

    #[test]
    fn you_can_pass_a_closure_with_empty_args_to_a_function() {
        assert_lines_equal!(
            r#"
                let run = (fn) {
                    println "Running..."
                    fn()
                }

                run {
                    print "Hello world"
                }
            "#,
            vec!["Running...", "Hello world"]
        );
    }

    #[test]
    fn you_can_pass_an_argumentless_closure_to_a_function() {
        assert_lines_equal!(
            r#"
                let run = (fn) {
                    println "Running..."
                    fn()
                }

                run {
                    print "Hello world"
                }
            "#,
            vec!["Running...", "Hello world"]
        );
    }

    #[test]
    fn you_can_evaluate_a_block_lazily() {
        assert_lines_equal!(
            r#"
                let my_lazy_thing = lazy {
                    println "resolving"
                    ret "result"
                }
                println "assigned"
                println my_lazy_thing
            "#,
            vec!["assigned", "resolving", "result"]
        );
    }

    #[test]
    fn you_can_evaluate_a_computation_lazily() {
        assert_lines_equal!(
            r#"
                let the_func = {
                    println "resolving"
                    ret "result"
                }
                let my_lazy_thing = lazy the_func()
                println "assigned"
                println my_lazy_thing
            "#,
            vec!["assigned", "resolving", "result"]
        );
    }

    #[test]
    fn you_can_escape_quotes_in_a_string() {
        assert_lines_equal!(
            r#"
                println "\""
            "#,
            vec!["\""]
        );
    }

    #[test]
    fn you_can_escape_tabs_in_a_string() {
        assert_lines_equal!(
            r#"
                println "\t"
            "#,
            vec!["\t"]
        );
    }

    //#[test]
    fn you_can_check_if_something_has_a_key_using_in() {
        assert_lines_equal!(
            r#"
                let my_map = Map()
                my_map.hello = "world"

                println Map.has(my_map, "hello")
                println Map.has(my_map, "there")
            "#,
            vec!["true", "false"]
        );
    }

    #[test]
    fn you_can_return_from_a_while() {
        assert_lines_equal!(
            r#"
                let func = () {
                    let i = 0

                    while lt(i, 10) {
                        i = add(i, 1)

                        if gt(i, 5) {
                            ret "early"
                        }

                        println i
                    }
                }

                println func()
            "#,
            vec!["1", "2", "3", "4", "5", "early"]
        );
    }

    #[test]
    fn you_can_call_deeply_nested_methods() {
        assert_lines_equal!(
            r#"
                let obj = Map()
                obj.foo = Map()
                obj.foo.bar = Map()
                obj.foo.bar.baz = Map()
                obj.foo.bar.baz.bax = () { "Hey" }

                println obj.foo.bar.baz.bax()
                println(obj.foo.bar.baz.bax())
            "#,
            vec!["Hey", "Hey"]
        );
    }

    #[test]
    fn you_can_access_deeply_nested_properties() {
        assert_lines_equal!(
            r#"
                let obj = Map()
                obj.foo = Map()
                obj.foo.bar = Map()
                obj.foo.bar.baz = Map()
                obj.foo.bar.baz.bax = "Hey"

                println obj.foo.bar.baz.bax
                println(obj.foo.bar.baz.bax)
            "#,
            vec!["Hey", "Hey"]
        );
    }

    #[test]
    fn you_can_multiply_numbers() {
        assert_lines_equal!(
            r#"
                let n = *(2, 3)

                print n
            "#,
            vec!["6"]
        );
    }

    #[test]
    fn you_can_specify_touples_without_parens_aka_simple_touples() {
        assert_lines_equal!(
            r#"
                let foo = "hey"
                print "a", "b", "c", 123, foo
            "#,
            vec!["abc123hey"]
        );
    }

    #[test]
    fn you_can_call_functions_in_simple_touples() {
        assert_lines_equal!(
            r#"
                let foo = "hey"
                let b = () {
                    "b"
                }
                print "a", b(), "c", 123, foo
            "#,
            vec!["abc123hey"]
        );
    }
}
