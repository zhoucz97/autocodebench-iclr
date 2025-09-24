func maxWordCount(_ sentences: [String]) -> Int {
    var maxCount = 0
    
    for sentence in sentences {
        let words = sentence.components(separatedBy: " ")
        let wordCount = words.count
        if wordCount > maxCount {
            maxCount = wordCount
        }
    }
    
    return maxCount
}


func checkMaxWordCount(_ maxWordCount: ([String]) -> Int) {
assert(maxWordCount(["Hello world", "Swift is amazing"]) == 3)
assert(maxWordCount(["The quick brown fox", "jumps over the lazy dog"]) == 5)
assert(maxWordCount(["single"]) == 1)
assert(maxWordCount(["", "one two", "one two three"]) == 3)
assert(maxWordCount(["This is a test sentence", "This is another"]) == 5)
assert(maxWordCount(["Sentence with five words", "Shorter one"]) == 4)
}

checkMaxWordCount(maxWordCount)