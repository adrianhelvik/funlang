let NameEchoer = (name) {
  return (cmd) {
    if eq(cmd, "sayName") {
        println name
    }
    if eq(cmd, "bark") {
        println "Woof!"
    }
  }
}

let dog = NameEchoer("Fido")

dog("sayName")
dog("bark")
