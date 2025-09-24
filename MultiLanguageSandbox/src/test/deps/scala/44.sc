

// Necessary imports
import scala.annotation.tailrec

object Solution {
  /**
     * Returns the least common multiple of 2 and n.
     *
     * @param n A positive integer.
     * @return The least common multiple of 2 and n.
     *
     * Example:
     * leastCommonMultiple(5)
     * Output: 10
     *
     * Example:
     * leastCommonMultiple(6)
     * Output: 6
     */
  def leastCommonMultiple(n: Int): Int = {
  // The LCM of two numbers can be calculated using the formula:
  // LCM(a, b) = (a * b) / GCD(a, b)
  val gcdValue = gcd(2, n)
  (2 * n) / gcdValue
}

// Helper function to calculate the greatest common divisor (GCD) using Euclid's algorithm
def gcd(a: Int, b: Int): Int = {
  if (b == 0) a else gcd(b, a % b)
}
    def main(args: Array[String]): Unit = {
    // Unit tests for the leastCommonMultiple function
    assert(leastCommonMultiple(3) == 6, "Test case 1 failed")
    assert(leastCommonMultiple(7) == 14, "Test case 2 failed")
    assert(leastCommonMultiple(10) == 10, "Test case 3 failed")
    
    println("All tests passed")
  }
}