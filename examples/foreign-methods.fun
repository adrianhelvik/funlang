from "core/hashmap" import HashMap
from "core/list" import List, map
from "core/lambda" import papp

let map_plain = fn list, fn {
    return object {
        next = fn {
            fn(item)
        }
    }
}

let map = fn.generator list, fn {
    let result = list()
    for item in list {
        yield fn(item);
    }
}

let square = fn values: List<i32> {
    values::map(fn it { it ** 2 })
}

List(1, 2, 3)
    ::map(fn it { list(it, square(it)) })
    ::map(fn list(before, after) { println(before, "squared is", after) })

HashMap(
    pair(1, square(1))
    pair(2, square(2))
    pair(3, square(3))
)
