func remainingApples(_ m: Int, _ t: Int, _ s: Int) -> Int {
    let eaten = min(m, s / t)
    return m - eaten
}


func check(_ remainingApples: (Int, Int, Int) -> Int) {
assert(remainingApples(50, 10, 200) == 30)
assert(remainingApples(100, 5, 1000) == 0)
assert(remainingApples(75, 15, 300) == 55)
assert(remainingApples(20, 20, 400) == 0)
assert(remainingApples(30, 10, 300) == 0)
assert(remainingApples(40, 15, 600) == 0)
assert(remainingApples(60, 10, 600) == 0)
}

check(remainingApples)