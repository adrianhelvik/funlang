println""
println"------------------------------"
println"Euler 03: Largest Prime Factor"
println"------------------------------"
println""

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

sieve(10000)
