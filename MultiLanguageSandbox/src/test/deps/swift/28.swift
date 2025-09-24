

func testCheckLikes(_ checkLikes: (Int) -> (Int, Int, Int, Int)) {
assert(checkLikes(12) == (0, 1, 1, 0))
assert(checkLikes(5) == (0, 1, 1, 0))
assert(checkLikes(6) == (1, 1, 0, 0))
assert(checkLikes(3) == (0, 0, 0, 1))
assert(checkLikes(13) == (0, 0, 0, 1))
assert(checkLikes(4) == (0, 1, 1, 0))
assert(checkLikes(11) == (0, 1, 1, 0))
assert(checkLikes(7) == (0, 1, 1, 0))
assert(checkLikes(2) == (0, 1, 1, 0))
}

testCheckLikes(checkLikes)