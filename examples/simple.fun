let x = 1

let touple = (x, 1)

x = add (x, 1)

let display_x = {
  println x
}

display_x()
display_x()
display_x()

let mult = (a, b) {
  if eq(b, 1) {
    return 1
  }
  return add(a, mult(a, b - 1))
}

println mult(5, 5)
