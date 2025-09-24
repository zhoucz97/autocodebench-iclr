

object Solution {
  /**
    * Calculate the total number of matches in a tournament with n teams.
    *
    * @param n The number of teams in the tournament.
    * @return The total number of matches.
    *
    * Example:
    * numberOfMatches(7)
    * Output: 6
    */
  def numberOfMatches(n: Int): Int = {
    if (n <= 1) 0
    else n - 1
}
  def main(args: Array[String]): Unit = {
    assert(numberOfMatches(8) == 7)
    assert(numberOfMatches(10) == 9)
    assert(numberOfMatches(12) == 11)
    println("All tests passed")
  }
}