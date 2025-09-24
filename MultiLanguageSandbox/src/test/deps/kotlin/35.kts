
/**
    * Calculates the number of days between two given dates.
    * The dates are provided as year, month, and day.
    * Accounts for leap years in the calculation.
    *
    * Example:
    * >>> daysBetweenDates(2008, 1, 1, 2009, 1, 1)
    *     366
    * >>> daysBetweenDates(2010, 1, 1, 2010, 1, 3)
    *     2
**/



fun daysBetweenDates(startYear: Int, startMonth: Int, startDay: Int, endYear: Int, endMonth: Int, endDay: Int): Int
{
    return daysSinceEpoch(endYear, endMonth, endDay) - daysSinceEpoch(startYear, startMonth, startDay)
}

private fun daysSinceEpoch(year: Int, month: Int, day: Int): Int {
    var totalDays = 0
    // Add all full years before the current year
    for (y in 0 until year) {
        totalDays += if (isLeapYear(y)) 366 else 365
    }
    // Add all full months before the current month in the current year
    for (m in 1 until month) {
        totalDays += daysInMonth(y, m)
    }
    // Add the days in the current month
    totalDays += day
    return totalDays
}

private fun isLeapYear(year: Int): Boolean {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

private fun daysInMonth(year: Int, month: Int): Int {
    return when (month) {
        1, 3, 5, 7, 8, 10, 12 -> 31
        4, 6, 9, 11 -> 30
        2 -> if (isLeapYear(year)) 29 else 28
        else -> throw IllegalArgumentException("Invalid month: $month")
    }
}
fun main() {
    check(daysBetweenDates(2008, 1, 1, 2009, 1, 1) == 366)
    check(daysBetweenDates(2010, 1, 1, 2010, 1, 3) == 2)
    check(daysBetweenDates(2020, 5, 15, 2020, 5, 20) == 5)
    check(daysBetweenDates(2019, 12, 31, 2020, 1, 1) == 1)
}


main()