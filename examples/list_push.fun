let items = [1, 2, 3]

for i in 4..10 {
    items.push(i)
}

print   "Expected: "
println "[1, 2, 3, 4, 5, 6, 7, 8, 9]"
println()
print   "Actual:   "
println items
