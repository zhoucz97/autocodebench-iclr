func calculateFinalScore(_ scores: [Double]) -> Double {
    guard scores.count >= 3 else {
        // If there are fewer than 3 scores, we can't remove both highest and lowest
        // In this case, we'll return the average of all available scores
        let sum = scores.reduce(0, +)
        return round(sum / Double(scores.count) * 100) / 100
    }
    
    let sortedScores = scores.sorted()
    let trimmedScores = Array(sortedScores.dropFirst().dropLast())
    let sum = trimmedScores.reduce(0, +)
    let average = sum / Double(trimmedScores.count)
    return round(average * 100) / 100
}


func testCalculateFinalScore() {
assert(calculateFinalScore([10.0, 8.5, 9.0, 9.5, 10.0]) == 9.50)
assert(calculateFinalScore([6.0, 7.5, 8.0, 7.0, 6.5]) == 7.00)
assert(calculateFinalScore([5.0, 5.5, 6.0, 5.5, 5.0]) == 5.33)
assert(calculateFinalScore([]) == 0.0)
}

testCalculateFinalScore()