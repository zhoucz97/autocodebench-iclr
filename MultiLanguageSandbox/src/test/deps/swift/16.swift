func closestToZero(_ nums: [Int]) -> Int {
    guard !nums.isEmpty else { return 0 } // handle empty array case
    
    var closest = nums[0]
    
    for num in nums {
        let absNum = abs(num)
        let absClosest = abs(closest)
        
        if absNum < absClosest {
            closest = num
        } else if absNum == absClosest {
            if num > closest {
                closest = num
            }
        }
    }
    
    return closest
}


func checkClosestToZero(_ closestToZero: ([Int]) -> Int) {
assert(closestToZero([2, -1, 1]) == 1)
assert(closestToZero([-3, -2, -1]) == -1)
assert(closestToZero([0, 10, 20]) == 0)
assert(closestToZero([5, 6, 7]) == 5)
assert(closestToZero([-5, 5]) == 5)
assert(closestToZero([-10, -5, 0, 5, 10]) == 0)
assert(closestToZero([]) == 0)
}

checkClosestToZero(closestToZero)