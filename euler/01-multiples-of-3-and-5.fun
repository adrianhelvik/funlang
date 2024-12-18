println ""
println "------------------------------"
println "Euler 01: Multiples of 3 and 5"
println "------------------------------"
println ""

let sum = 0

for i in 0..1000 {
    if or(eq(modulo(i, 3), 0), eq(modulo(i, 5), 0)) {
        sum = add(sum, i)
    }
}

println "sum: ", sum
