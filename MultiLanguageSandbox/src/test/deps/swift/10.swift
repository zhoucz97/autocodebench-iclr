func countPrefixStrings(_ words: [String], _ pref: String) -> Int {
    return words.filter { $0.hasPrefix(pref) }.count
}


func checkCountPrefixStrings(_ countPrefixStrings: ([String], String) -> Int) {
assert(countPrefixStrings(["apple", "app", "application", "orange"], "app") == 3)
assert(countPrefixStrings(["hello", "world"], "he") == 1)
assert(countPrefixStrings(["python", "php", "perl", "java"], "p") == 3)
assert(countPrefixStrings(["swift", "scala"], "s") == 2)
assert(countPrefixStrings(["go", "rust", "ruby"], "c") == 0)
assert(countPrefixStrings([], "test") == 0)
assert(countPrefixStrings(["prefix", "preference", "preliminary"], "pre") == 3)
}

checkCountPrefixStrings(countPrefixStrings)