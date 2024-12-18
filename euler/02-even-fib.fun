println ""
println "--------------------------------"
println "Euler 02: Even Fibonacci numbers"
println "--------------------------------"
println ""

let sum = 0

let cached = (func) {
    let cache = Map()

    (n) {
        let value = cache(n)
        if not eq(value, null) {
            ret cache(n)
        }
        let value = func(n)
        cache(n, value)
        ret value
    }
}

let fib = cached (n) {
    if lte(n, 1) {
        1
    } else {
        add(fib(-(n, 1)), fib(-(n, 2)))
    }
}

let sum = 0
let i = 0
let value = fib i
while lt(value, 4000000) {
    i = add(i, 1)
    value = fib i

    if eq(modulo(value, 2), 0) {
        sum = add(sum, value)
    }
}

println "Sum:      ", sum
println "Expected: ", 4613732
