let sum = 0

for(i, 0, 1000, {
    if or(eq(modulo(i, 3), 0), eq(modulo(i, 5), 0)) {
        println i
        sum = add(sum, i)
    }
})

println("sum: ", sum)
