func isLuckyWord(_ word: String) -> (isLucky: Bool, difference: Int) {
    // Calculate character frequencies
    var frequencyDict = [Character: Int]()
    for char in word {
        frequencyDict[char] = (frequencyDict[char] ?? 0) + 1
    }
    
    // Get all frequencies
    guard let frequencies = frequencyDict.values.isEmpty ? nil : Array(frequencyDict.values) else {
        return (false, 0)
    }
    
    // Find max and min frequencies
    let maxFrequency = frequencies.max()!
    let minFrequency = frequencies.min()!
    let difference = maxFrequency - minFrequency
    
    // Check if difference is prime
    func isPrime(_ n: Int) -> Bool {
        if n <= 1 {
            return false
        }
        if n <= 3 {
            return true
        }
        if n % 2 == 0 || n % 3 == 0 {
            return false
        }
        var i = 5
        var w = 2
        while i * i <= n {
            if n % i == 0 {
                return false
            }
            i += w
            w = 6 - w
        }
        return true
    }
    
    let isLucky = isPrime(difference)
    return (isLucky, isLucky ? difference : 0)
}


func testIsLuckyWord() {
assert(isLuckyWord("hello") == (false, 0))
assert(isLuckyWord("swift") == (false, 0))
assert(isLuckyWord("programming") == (false, 0))
assert(isLuckyWord("apple") == (false, 0))
assert(isLuckyWord("banana") == (true, 2))
assert(isLuckyWord("challenge") == (false, 0))
assert(isLuckyWord("developer") == (true, 2))
}

testIsLuckyWord()