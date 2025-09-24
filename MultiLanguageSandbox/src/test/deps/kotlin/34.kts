import java.time.LocalDate
import java.time.format.DateTimeFormatter
/**
    * Calculates the date of the next day based on a given date in the format yyyy-mm-dd.
    * Assumes valid input dates ranging from 1600-01-01 to 2999-12-30.
    *
    * Example:
    * >>> getNextDayDate("2010-07-05")
    *     "2010-07-06"
    * >>> getNextDayDate("2020-02-28")
    *     "2020-02-29"  // For a leap year
    * >>> getNextDayDate("2020-12-31")
    *     "2021-01-01"  // For end of the year
**/

fun getNextDayDate(currentDate: String): String 
{
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(currentDate, formatter)
    val nextDay = date.plusDays(1)
    return nextDay.format(formatter)
}
fun main() {
    check(getNextDayDate("2010-07-05") == "2010-07-06")
    check(getNextDayDate("2020-02-28") == "2020-02-29") // Leap year case
    check(getNextDayDate("2020-12-31") == "2021-01-01") // End of the year case
    check(getNextDayDate("2019-02-28") == "2019-03-01") // Non-leap year February case
}

main()