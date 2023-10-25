let fib = (n) {
    print "n is now " println n
    if eq(n, 0) {
        println "base case"
        ret 0
    }
    if eq(n, 1) {
        println "base case"
        ret 1
    }
    let minus_one = sub(n, 1)
    let minus_two = sub(n, 2)
    let a = fib minus_one
    let b = fib minus_two

    ret add(a, b)
}

println(fib 10)
