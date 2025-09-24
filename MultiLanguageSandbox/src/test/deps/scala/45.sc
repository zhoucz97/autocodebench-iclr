

object Solution {
    /**
       * Calculate the Hamming distance between two integers.
       *
       * @param x The first integer.
       * @param y The second integer.
       * @return The Hamming distance between x and y.
       *
       * Example:
       * hammingDistance(1, 4)
       * Output: 2
       *
       * hammingDistance(3, 1)
       * Output: 1
       */
  def hammingDistance(x: Int, y: Int): Int = {
  // XOR the two numbers to get a number where bits are set to 1 where they differ
  val xorResult = x ^ y
  // Count the number of 1s in the binary representation of the XOR result
  Integer.bitCount(xorResult)
}
  def main(args: Array[String]): Unit = {
    assert(hammingDistance(5, 9) == 2) // 0101 vs 1001 -> 3 differences
    assert(hammingDistance(10, 20) == 4) // 01010 vs 10100 -> 4 differences
    assert(hammingDistance(17, 34) == 4) // 010001 vs 100010 -> 3 differences
    println("All tests passed")
  }
}