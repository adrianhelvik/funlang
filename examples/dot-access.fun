let Animal = () {
    let animal = Map()

    animal.name = "Furry"

    animal.say_name = () {
        println animal.name
    }

    animal.set_name = (name) {
        animal.name = name
    }

    ret animal
}

let animal = Animal()
animal.say_name()

animal.set_name("Fluffy")
animal.say_name()
