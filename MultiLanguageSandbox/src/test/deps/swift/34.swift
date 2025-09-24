func convertNumber(fromBase: Int, number: String, toBase: Int) -> String {
    // First, convert the input number to decimal (base 10)
    guard let decimalValue = Int(number, radix: fromBase) else {
        return "Invalid input number for base \(fromBase)"
    }
    
    // Handle the special case of converting to base 10
    if toBase == 10 {
        return String(decimalValue)
    }
    
    // Convert the decimal value to the target base
    var result = ""
    var value = decimalValue
    let digits = "0123456789ABCDEF"
    
    while value > 0 {
        let remainder = value % toBase
        result = String(digits[remainder]) + result
        value /= toBase
    }
    
    // Handle the case when the input number is 0
    return result.isEmpty ? "0" : result
}


func check(_ convertNumber: (Int, String, Int) -> String) {
assert(convertNumber(10, "255", 2) == "11111111")
assert(convertNumber(2, "1010", 10) == "10")
assert(convertNumber(8, "175", 16) == "7D")
assert(convertNumber(16, "1A", 10) == "26")
assert(convertNumber(2, "1111", 16) == "F")
assert(convertNumber(16, "FF", 8) == "377")
assert(convertNumber(10, "1000", 16) == "3E8")
}

check(convertNumber)