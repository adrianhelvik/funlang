let Animal = () {
    let animal = Map()

    animal.name = "Furry"

    animal.say_name = () {
        println animal.name
    }

    ret animal
}

let animal = Animal()
let say_name = animal.say_name
say_name()
animal.say_name()
