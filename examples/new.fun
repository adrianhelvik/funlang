# We can specify explicit types, like this,
# which also shows that lists are just a
# mapping.
let numbers: (i: usize) -> i32? = List(1, 2, 3, 4, 5)

# Or we can rely on type inference
let numbers = List(1, 2, 3, 4, 5)

# We can return from expressions, and we can
# return within a file as well as within function.
# A return expression simply returns from the
# nearest function or file context.
let n: i32 = numbers(0) ?? return
