

object Solution {
  /**
   * Calculate the time needed to obtain the reverse seating order.
   * For each person, the person who was originally on their left will be on their right, and the person who was originally on their right will be on their left.
   * 
   * @param N the number of people
   * @return the time needed in minutes
   * 
   * Example:
   * >>> calculateTimeNeeded(4)
   * 2
   * >>> calculateTimeNeeded(5)
   * 4
   * >>> calculateTimeNeeded(6)
   * 6
   */
  def calculateTimeNeeded(N: Int): Int = {
    if (N % 2 == 0) {
        N / 2
    } else {
        N - 1
    }
}
  def main(args: Array[String]): Unit = {
    // Test the function with different test cases
    assert(calculateTimeNeeded(7) == 3)
    assert(calculateTimeNeeded(8) == 4)
    assert(calculateTimeNeeded(9) == 4)
    println("All tests passed")
  }
}