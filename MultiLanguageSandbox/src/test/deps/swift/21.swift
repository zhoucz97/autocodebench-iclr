func findFourthVertex(_ vertex1: (Int, Int), _ vertex2: (Int, Int), _ vertex3: (Int, Int)) -> (Int, Int) {
    let xCoords = [vertex1.0, vertex2.0, vertex3.0]
    let yCoords = [vertex1.1, vertex2.1, vertex3.1]
    
    var xCount = [Int: Int]()
    var yCount = [Int: Int]()
    
    for x in xCoords {
        xCount[x] = (xCount[x] ?? 0) + 1
    }
    
    for y in yCoords {
        yCount[y] = (yCount[y] ?? 0) + 1
    }
    
    let fourthX = xCount.first { $0.value == 1 }!.key
    let fourthY = yCount.first { $0.value == 1 }!.key
    
    return (fourthX, fourthY)
}


func check(_ findFourthVertex: ((Int, Int), (Int, Int), (Int, Int)) -> (Int, Int)) {
assert(findFourthVertex((5, 5), (5, 7), (7, 5)) == (7, 7))
assert(findFourthVertex((2, 3), (2, 5), (4, 5)) == (4, 3))
assert(findFourthVertex((10, 10), (10, 20), (20, 10)) == (20, 20))
assert(findFourthVertex((15, 15), (10, 15), (10, 10)) == (15, 10))
assert(findFourthVertex((3, 4), (5, 4), (3, 2)) == (5, 2))
assert(findFourthVertex((8, 9), (8, 12), (11, 12)) == (11, 9))
}

check(findFourthVertex)