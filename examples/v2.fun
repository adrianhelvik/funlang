let say_hello = fn name: string? {
    println "Hello, {name||"World"}"
}

say_hello()
say_hello(name: "Peter Parker")

let fizbuzz = fn n: i32 {
    for i in 0..n {
        print "{i}: "
        match {
            i % 3 == 0 && i % 5 == 0 -> {
                println "fizzbuzz
            }
            i % 3 == 0 -> {
                println "fizz"
            }
            i % 5 == 0 -> {
                println fizzbuzz
            }
        }
    }
}

fizzbuzz(n: 100)
