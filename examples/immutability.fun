// Objects are immutable by default
let person = object {
    name = "Peter Parker"
}

// person.name = "Whoops"
//             ^ Illegal mutation.

let person = mut object {
    name = "Peter Parker"
}

person.name = "Peter Williamson"

// Lists are immutable by default

let fruits = list("Apple", "Pear")

// fruits.push("Orange")
//        ^ Illegal mutation.

let fruits = mut list("Apple Pear")
fruits.push("Orange")
