println""
println"------------------------------"
println"Euler 01: Multiples of 3 and 5"
println"------------------------------"

let sum = 0

for i in 0..1000 {
    println""
    println("For i = ", i)
    println("i % 3 = ", modulo(i, 3))
    println("i % 3 == 0: ", eq(modulo(i, 3), 0))

    if or(eq(modulo(i, 3), 0), eq(modulo(i, 5), 0)) {
        println i
        sum = add(sum, i)
    }
}

println""
println("sum: ", sum)
