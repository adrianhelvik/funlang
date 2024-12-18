println ""
println "-------------------------------"
println "Euler 06: Sum Square Difference"
println "-------------------------------"
println ""

let square = (n) {
    mul(n, n)
}

let number_list = (n) {
    let list = []
    for i in 0..n {
        list.push(+(i, 1))
    }
    list
}

let sum = (list) {
    let sum = 0
    for i in 0..list.len() {
        sum = +(sum, list(i))
    }
    sum
}

let squared_sum = (n) {
    let sum = 0
    for i in 1..+(n, 1) {
        sum = +(sum, square(i))
    }
    sum
}

let main = (n) {
    let a = squared_sum n
    let b = square(sum number_list n)
    let diff = -(b, a)

    println("sum of squares: ", a)
    println("   squared sum: ", b)
    println("          diff: ", diff)
}

println "First off.. The example:"
println ""

main 10

println ""
println "Then, the real deal"
println ""

main 100
