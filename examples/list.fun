println""
println"------------"
println"List example"
println"------------"

let list = () {
    let self = Map()
    let len = 0

    self.insert = (item) {
        self(str(len), item)
        len = add(len, 1)
    }

    self.len = () {
        ret len
    }

    self.each = (fn) {
        for i in 0..len {
            fn(self(i), i)
        }
    }

    ret self
}

let list = list()

list.insert(10)
list.insert(1337)
list.insert(42)

println list.len()

list.each((item, index) {
    println("item ", index, ": ", item)
})
