import scala.math.sqrt


object Solution {
  /**
    * Given a total number of n coins, you plan to arrange them in a staircase pattern. Each row must have exactly the same number of coins as its row index.
    * Calculate and return the total number of complete rows that can be formed.
    *
    * @param n The total number of coins.
    * @return The total number of complete rows that can be formed.
    *
    * Example:
    * arrangeCoins(5)
    * Output: 2
    * Explanation: The third row is incomplete, so return 2.
    */
  def arrangeCoins(n: Int): Int = {
    val k = (-1 + sqrt(1 + 8 * n)) / 2
    k.toInt
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(arrangeCoins(8) == 3)
    assert(arrangeCoins(15) == 5)
    assert(arrangeCoins(20) == 5)
    println("All tests passed")
  }
}