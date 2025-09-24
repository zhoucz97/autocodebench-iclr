func minimumSwapsToOrder(_ trainCars: [Int]) -> Int {
    var cars = trainCars
    let n = cars.count
    var swaps = 0
    
    while true {
        // Check if the array is sorted
        var isSorted = true
        for i in 0..<n-1 {
            if cars[i] > cars[i+1] {
                isSorted = false
                break
            }
        }
        if isSorted {
            return swaps
        }
        
        // Perform a 180-degree rotation
        var rotated = [Int](repeating: 0, count: n)
        for i in 0..<n {
            rotated[i] = cars[n - 1 - i]
        }
        cars = rotated
        swaps += 1
        
        // To prevent infinite loops in case it's impossible (though problem says it's possible)
        if swaps > n * 2 {
            return -1 // though per problem statement, it's solvable
        }
    }
}


func check(_ minimumSwapsToOrder: ([Int]) -> Int) {
assert(minimumSwapsToOrder([2, 3, 1]) == 2)
assert(minimumSwapsToOrder([1, 4, 3, 2]) == 3)
assert(minimumSwapsToOrder([5, 4, 3, 2, 1]) == 10)
assert(minimumSwapsToOrder([1, 2, 3, 4, 5]) == 0)
assert(minimumSwapsToOrder([3, 1, 2]) == 2)
assert(minimumSwapsToOrder([4, 5, 1, 3, 2]) == 7)
assert(minimumSwapsToOrder([1]) == 0)
}

check(minimumSwapsToOrder)