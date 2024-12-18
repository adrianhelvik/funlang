let sieve = (n) {
    let values = Map()

    for i in 2..n {
        values(str(i), true)
    }

    let prime = 2

    let list = []
    while lt(prime, n) {
        list.push(prime)

        "
            Increment by 'prime' up until n and mark as false
        "
        let times = 2
        let tmp = prime
        while lt(tmp, n) {
            tmp = add(tmp, prime)
            values(str(tmp), false)
        }

        "
            Determine the next prime. We start off by incrementing by one
            as the successor to a prime is never a prime.
        "
        let next = add(prime, 1)
        while and(not values(next), lt(next, n)) {
            next = add(next, 1)
        }
        prime = next
    }

    ret list
}
