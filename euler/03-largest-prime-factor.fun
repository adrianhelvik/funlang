println""
println"------------------------------"
println"Euler 03: Largest Prime Factor"
println"------------------------------"
println""

from "./sieve.fun" import sieve

let primes = sieve 13195

println "Available primes: ", primes.len()
