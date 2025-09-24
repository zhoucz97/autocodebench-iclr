func maxChessboardSlices(_ n: Int) -> Int {
    return (n * n + n + 2) / 2
}


func testMaxChessboardSlices() {
assert(maxChessboardSlices(0) == 1)
assert(maxChessboardSlices(2) == 4)
assert(maxChessboardSlices(4) == 9)
assert(maxChessboardSlices(5) == 12)
assert(maxChessboardSlices(6) == 16)
assert(maxChessboardSlices(7) == 20)
}

testMaxChessboardSlices()