func countPrefixes(_ words: [String], _ s: String) -> Int {
    var count = 0
    for word in words {
        if s.hasPrefix(word) {
            count += 1
        }
    }
    return count
}


func checkCountPrefixes(_ countPrefixes: ([String], String) -> Int) {
assert(countPrefixes(["a", "b", "c", "ab"], "abc") == 2)
assert(countPrefixes(["hello", "hell", "heaven", "goodbye"], "hello") == 2)
assert(countPrefixes(["app", "apple", "apricot", "ap"], "apple") == 3)
assert(countPrefixes(["blue", "blues", "blueberry"], "blue") == 1)
assert(countPrefixes(["no", "match"], "yes") == 0)
assert(countPrefixes([], "empty") == 0)
assert(countPrefixes(["full", "fullest", "ful"], "fullest") == 3)
}

checkCountPrefixes(countPrefixes)