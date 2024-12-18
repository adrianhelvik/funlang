println ""
println "----------------------"
println "Euler 07: 1001st prime"
println "----------------------"
println ""

from "./sieve.fun" import sieve

"
    Since the sieve function I've already made
    accepts a limit, I just chose an arbitrary
    high number until there were enough primes.

    Not the most elegant solutation, but it works.
"

let primes = sieve 120000

println primes(10000)
