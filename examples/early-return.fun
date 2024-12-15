let func = () {
    println 1
    if eq(1, 1) {
        println 2
        ret "inner"
    }
    println 3
    ret "outer"
}

println func()
