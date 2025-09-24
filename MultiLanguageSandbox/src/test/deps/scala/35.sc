

object Solution {
  /**
    * Check if every integer in the range [1, n] has a binary representation that is a substring of the given binary string.
    *
    * @param s The binary string.
    * @param n The positive integer.
    * @return True if every integer in the range [1, n] has a binary representation that is a substring of the given binary string, false otherwise.
    *
    * Example:
    * queryString("0110", 3)
    * Output: true
    */
  def queryString(s: String, n: Int): Boolean = {
    (1 to n).forall { num =>
        val binary = num.toBinaryString
        s.contains(binary)
    }
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(queryString("0110", 3))
    assert(!queryString("0110", 4))
    assert(queryString("1111000101", 5))

    println("All tests passed")
  }
}