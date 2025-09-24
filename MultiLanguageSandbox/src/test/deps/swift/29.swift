func isLeapYear(_ year: Int) -> Int {
    if year % 4 != 0 {
        return 0
    } else if year % 100 != 0 {
        return 1
    } else {
        return year % 400 == 0 ? 1 : 0
    }
}


func check(_ isLeapYear: (Int) -> Int) {
assert(isLeapYear(1926) == 0)
assert(isLeapYear(1900) == 0)
assert(isLeapYear(2000) == 1)
assert(isLeapYear(1996) == 1)
assert(isLeapYear(2020) == 1)
assert(isLeapYear(2021) == 0)
assert(isLeapYear(2022) == 0)
assert(isLeapYear(2023) == 0)
assert(isLeapYear(2024) == 1)
}

check(isLeapYear)