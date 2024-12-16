println""
println"------------------------------"
println"Euler 03: Largest Prime Factor"
println"------------------------------"
println""

from "../lib/list.fun" import list

let sieve = (n) {
    let values = Map()

    for i in 2..n {
        values(str(i), true)
    }

    let prime = 2
    while lt(prime, n) {
        println("prime: ", prime)
        let i = 0
        while lt(i, n) {
            i = add(i, prime)
            values(str(i), false)
        }
        prime = add(prime, 1)
        while not values(prime) {
            prime = add(prime, 1)
        }
    }
}

let factors = (n) {
    for i in 0
}

eq(factors 13195, [5, 7, 13, 29])

sieve(10000)
