

object Solution {

  /**
   * Given an integer n, return the minimum number of perfect square numbers that sum up to n.
   *
   * @param n The target integer.
   * @return The minimum number of perfect square numbers.
   *
   * Example:
   * minPerfectSquareSum(12)
   * Output: 3
   *
   * Example:
   * minPerfectSquareSum(13)
   * Output: 2
   */
  def minPerfectSquareSum(n: Int): Int = {
    val dp = Array.fill(n + 1)(Int.MaxValue)
    dp(0) = 0
    
    for (i <- 1 to n) {
        var j = 1
        while (j * j <= i) {
            dp(i) = math.min(dp(i), dp(i - j * j) + 1)
            j += 1
        }
    }
    
    dp(n)
}
  def main(args: Array[String]): Unit = {
    assert(minPerfectSquareSum(26) == 2, "Test case 1 failed")
    assert(minPerfectSquareSum(17) == 2, "Test case 2 failed")
    assert(minPerfectSquareSum(99) == 3, "Test case 3 failed")
    println("All tests passed")
  }
}