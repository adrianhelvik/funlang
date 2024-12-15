println()
println"--------------------------------"
println"Euler 02: Even Fibonacci numbers"
println"--------------------------------"
println""

let sum = 0

let cached = (func) {
    let cache = Map()
    return (arg) {
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

let sum = 0
let i = 0
let next = () {
    let value = fib i
    i = add(i, 1)

    if gte(value, 4000000) {
        ret
    }

    if eq(modulo(value, 2), 0) {
        sum = add(sum, value)
    }

    next()
}
next()

println("Sum:      ", sum)
println("Expected: ", 4613732)
