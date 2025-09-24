import java.time.LocalDate
import java.time.temporal.ChronoField

object Solution {

    /**
     * Given a date in the format "YYYY-MM-DD", return the day of the year.
     *
     * @param date The date in the format "YYYY-MM-DD".
     * @return The day of the year.
     *
     * Example:
     * dayOfYear("2019-01-09")
     * Output: 9
     *
     * dayOfYear("2019-02-10")
     * Output: 41
     */
  def dayOfYear(date: String): Int = {
  val localDate = LocalDate.parse(date)
  localDate.get(ChronoField.DAY_OF_YEAR)
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(dayOfYear("2022-03-15") == 74)
    assert(dayOfYear("2023-11-30") == 334)
    assert(dayOfYear("2024-07-01") == 183)

    println("All tests passed")
  }
}