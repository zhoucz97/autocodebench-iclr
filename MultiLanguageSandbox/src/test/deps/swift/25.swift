func findMissingNumber(in numbers: [Int]) -> Int {
    let sortedNumbers = numbers.sorted()
    let n = sortedNumbers.count
    
    // The original sequence had 4 terms, now 3 remain. The missing term could be:
    // 1. The first term (a)
    // 2. The second term (a + d)
    // 3. The third term (a + 2d)
    // 4. The fourth term (a + 3d)
    
    // We need to check all possible cases where the missing term is one of these positions.
    
    // Case 1: Missing term is the first term (a)
    // The remaining terms are a+d, a+2d, a+3d. So d can be (sortedNumbers[1] - sortedNumbers[0]) or (sortedNumbers[2] - sortedNumbers[1])
    // But since it's AP, the differences should be consistent. So d is (sortedNumbers[1] - sortedNumbers[0]) == (sortedNumbers[2] - sortedNumbers[1])? No, because one term is missing.
    // So for case 1, the three numbers are a+d, a+2d, a+3d. Then d = (a+2d - (a+d)) = sortedNumbers[1] - sortedNumbers[0]
    // So d = sortedNumbers[1] - sortedNumbers[0]
    // Then a = sortedNumbers[0] - d
    // Check if the sequence a, a+d, a+2d, a+3d includes all three numbers except a.
    let d1 = sortedNumbers[1] - sortedNumbers[0]
    let a1 = sortedNumbers[0] - d1
    let candidate1 = a1
    var tempSequence1 = [a1, sortedNumbers[0], sortedNumbers[1], sortedNumbers[2]]
    tempSequence1.sort()
    let originalSequence1 = [a1, a1 + d1, a1 + 2*d1, a1 + 3*d1].sorted()
    if tempSequence1 == originalSequence1 {
        return candidate1
    }
    
    // Case 2: Missing term is the second term (a + d)
    // The remaining terms are a, a+2d, a+3d. So d can be (a+3d - a)/2 = (sortedNumbers[2] - sortedNumbers[0])/2
    let d2 = (sortedNumbers[2] - sortedNumbers[0]) / 2
    let a2 = sortedNumbers[0]
    let candidate2 = a2 + d2
    var tempSequence2 = [a2, a2 + 2*d2, a2 + 3*d2, candidate2]
    tempSequence2.sort()
    let originalSequence2 = [a2, a2 + d2, a2 + 2*d2, a2 + 3*d2].sorted()
    if tempSequence2 == originalSequence2 {
        return candidate2
    }
    
    // Case 3: Missing term is the third term (a + 2d)
    // The remaining terms are a, a+d, a+3d. So d can be (a+3d - a)/3? No, because the missing term is a+2d. The three numbers are a, a+d, a+3d. So d is (a+d - a) = sortedNumbers[1] - sortedNumbers[0]
    let d3 = sortedNumbers[1] - sortedNumbers[0]
    let a3 = sortedNumbers[0]
    let candidate3 = a3 + 2 * d3
    var tempSequence3 = [a3, a3 + d3, a3 + 3*d3, candidate3]
    tempSequence3.sort()
    let originalSequence3 = [a3, a3 + d3, a3 + 2*d3, a3 + 3*d3].sorted()
    if tempSequence3 == originalSequence3 {
        return candidate3
    }
    
    // Case 4: Missing term is the fourth term (a + 3d)
    // The remaining terms are a, a+d, a+2d. So d is (a+2d - a)/2 = (sortedNumbers[2] - sortedNumbers[0])/2
    let d4 = (sortedNumbers[2] - sortedNumbers[0]) / 2
    let a4 = sortedNumbers[0]
    let candidate4 = a4 + 3 * d4
    var tempSequence4 = [a4, a4 + d4, a4 + 2*d4, candidate4]
    tempSequence4.sort()
    let originalSequence4 = [a4, a4 + d4, a4 + 2*d4, a4 + 3*d4].sorted()
    if tempSequence4 == originalSequence4 {
        return candidate4
    }
    
    // If none of the above cases match (which shouldn't happen as per problem statement), return -1 or handle error.
    // But according to the problem, there's exactly one missing number, so one of the cases must fit.
    return -1
}


func testFindMissingNumber() {
assert(findMissingNumber(in: [3, 7, 5]) == 1 || findMissingNumber(in: [3, 7, 5]) == 9)
assert(findMissingNumber(in: [20, 14, 8]) == 26 || findMissingNumber(in: [20, 14, 8]) == 2)
assert(findMissingNumber(in: [12, 6, 18]) == 0 || findMissingNumber(in: [12, 6, 18]) == 24)
assert(findMissingNumber(in: [-4, 0, -8]) == -12 || findMissingNumber(in: [-4, 0, -8]) == 4)
assert(findMissingNumber(in: [15, 5, 10]) == 0 || findMissingNumber(in: [15, 5, 10]) == 20)
}

testFindMissingNumber()