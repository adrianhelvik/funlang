println "How do you do?"
println "I am fine..."

println("a", "b", "c")
println("Hello", "world")
println("Here is a number ", add(1234, 10))

let x = 10

println("The number is: ", x)

println("The message outside print_message is: ", message)

let print_message = {
  let message = "Hello world"

  println("The message inside print_message is: ", message)
}

print_message()

println("The message outside print_message is: ", message)
