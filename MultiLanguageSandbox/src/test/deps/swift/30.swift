func daysInMonth(_ year: Int, _ month: Int) -> Int {
    // Check for invalid month
    guard month >= 1 && month <= 12 else {
        return 0
    }
    
    // Months with 31 days
    if month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 {
        return 31
    }
    // Months with 30 days
    else if month == 4 || month == 6 || month == 9 || month == 11 {
        return 30
    }
    // February - check for leap year
    else if month == 2 {
        // Leap year rules:
        // 1. Divisible by 4
        // 2. Not divisible by 100 unless also divisible by 400
        let isLeapYear = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
        return isLeapYear ? 29 : 28
    }
    
    // This line is theoretically unreachable due to the initial guard statement
    return 0
}


func testDaysInMonth() {
// Non-leap years
assert(daysInMonth(1926, 8) == 31)
assert(daysInMonth(1999, 12) == 31)

// Leap years
assert(daysInMonth(2000, 2) == 29)
assert(daysInMonth(2020, 2) == 29)

// Months with 30 days
assert(daysInMonth(2022, 4) == 30)
assert(daysInMonth(2022, 6) == 30)

// Months with 31 days
assert(daysInMonth(2022, 1) == 31)
assert(daysInMonth(2022, 3) == 31)
}

testDaysInMonth()