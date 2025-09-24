func countUnfairReminders(_ children: [String]) -> Int {
    var count = 0
    var childCounts = [String: Int]()
    
    for (index, child) in children.enumerated() {
        // The current count of the child before processing this entry
        let currentCount = childCounts[child] ?? 0
        
        // The adjusted count for the child (excluding the current portion)
        let adjustedCount = currentCount - 1
        
        // Sum of all other children's counts
        var sumOthers = 0
        for (otherChild, otherCount) in childCounts {
            if otherChild != child {
                sumOthers += otherCount
            }
        }
        
        // Check if the adjusted count is greater than the sum of others
        if adjustedCount > sumOthers {
            count += 1
        }
        
        // Update the child's count after processing
        childCounts[child] = currentCount + 1
    }
    
    return count
}


func testCountUnfairReminders() {
assert(countUnfairReminders(["mirko", "stanko", "stanko", "stanko"]) == 1)
assert(countUnfairReminders(["a", "b", "b", "a", "a", "a", "c", "a", "b", "b", "c", "b"]) == 0)
assert(countUnfairReminders(["a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a"]) == 10)
assert(countUnfairReminders(["a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a"]) == 0)
print("All test cases pass.")
}

testCountUnfairReminders()