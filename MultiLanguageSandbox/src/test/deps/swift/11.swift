func removeAnagramWords(_ words: [String]) -> [String] {
    var result = words
    var changed = true
    
    while changed {
        changed = false
        var i = 1
        while i < result.count {
            let current = result[i]
            let previous = result[i - 1]
            
            if areAnagrams(current, previous) {
                result.remove(at: i)
                changed = true
                // After removal, the next element (if any) is now at position i, so we don't increment i
            } else {
                i += 1
            }
        }
    }
    
    return result
}

func areAnagrams(_ s1: String, _ s2: String) -> Bool {
    guard s1.count == s2.count else {
        return false
    }
    let sortedS1 = s1.sorted()
    let sortedS2 = s2.sorted()
    return sortedS1 == sortedS2
}


func checkRemoveAnagramWords(_ removeAnagramWords: ([String]) -> [String]) {
assert(removeAnagramWords(["ab", "ba", "cd", "dc", "e"]) == ["ab", "cd", "e"])
assert(removeAnagramWords(["abc", "cba", "bac", "acb"]) == ["abc"])
assert(removeAnagramWords(["word", "drow", "hello", "world"]) == ["word", "hello","world"])
assert(removeAnagramWords(["a", "b", "c", "d"]) == ["a", "b", "c", "d"])
assert(removeAnagramWords(["zzz", "zzz", "zzz"]) == ["zzz"])
assert(removeAnagramWords(["abcd", "dbca", "dcba"]) == ["abcd"])
}

checkRemoveAnagramWords(removeAnagramWords)