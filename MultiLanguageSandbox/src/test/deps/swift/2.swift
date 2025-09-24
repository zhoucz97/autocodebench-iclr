func firstPalindrome(_ words: [String]) -> String {
    for word in words {
        if word == String(word.reversed()) {
            return word
        }
    }
    return ""
}


func checkFirstPalindrome(_ firstPalindrome: ([String]) -> String) {
assert(firstPalindrome(["apple", "banana", "civic"]) == "civic", "Test failed: Expected 'civic'")
assert(firstPalindrome(["abc", "car", "ada", "racecar"]) == "ada", "Test failed: Expected 'ada'")
assert(firstPalindrome(["hello", "world"]) == "", "Test failed: Expected an empty string")
assert(firstPalindrome(["radar", "rotor", "level"]) == "radar", "Test failed: Expected 'radar'")
assert(firstPalindrome(["not", "a", "single", "palindrome"]) == "a", "Test failed: Expected an empty string")
assert(firstPalindrome(["A", "B", "C", "D"]) == "A", "Test failed: Expected 'A'")
assert(firstPalindrome(["noon", "evening", "morning"]) == "noon", "Test failed: Expected 'noon'")
}


checkFirstPalindrome(firstPalindrome)