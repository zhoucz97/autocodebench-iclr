func mostFrequentTargetAfterKey(_ nums: [Int], _ key: Int) -> Int {
    var frequencyDict = [Int: Int]()
    
    for i in 0..<nums.count - 1 {
        if nums[i] == key {
            let target = nums[i + 1]
            frequencyDict[target] = (frequencyDict[target] ?? 0) + 1
        }
    }
    
    // Find the target with the highest frequency
    var mostFrequentTarget = 0
    var maxFrequency = 0
    
    for (target, frequency) in frequencyDict {
        if frequency > maxFrequency {
            maxFrequency = frequency
            mostFrequentTarget = target
        }
    }
    
    return mostFrequentTarget
}


func checkMostFrequentTargetAfterKey(_ mostFrequentTargetAfterKey: ([Int], Int) -> Int) {
assert(mostFrequentTargetAfterKey([1, 2, 1, 2, 3, 1, 2, 3], 1) == 2)
assert(mostFrequentTargetAfterKey([1, 1, 1, 2, 2, 2, 1, 3, 3], 1) == 1)
assert(mostFrequentTargetAfterKey([1, 2, 3, 1, 2, 3, 1, 2], 2) == 3)
assert(mostFrequentTargetAfterKey([7, 7, 7, 8, 8, 9, 7, 9, 9], 7) == 7)
assert(mostFrequentTargetAfterKey([1], 1) == 1)
assert(mostFrequentTargetAfterKey([2, 2, 2, 2, 2, 3], 2) == 2)
}

checkMostFrequentTargetAfterKey(mostFrequentTargetAfterKey)