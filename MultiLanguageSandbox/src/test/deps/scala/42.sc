


object Solution {
  /**
    * Check if the binary representation of a positive integer has alternating bits.
    *
    * @param n The positive integer to check.
    * @return True if the binary representation has alternating bits, false otherwise.
    *
    * Example:
    * hasAlternatingBits(5)
    * Output: true
    */
  def hasAlternatingBits(n: Int): Boolean = {
    var prevBit = n & 1
    var num = n >> 1
    while (num > 0) {
        val currentBit = num & 1
        if (currentBit == prevBit) {
            return false
        }
        prevBit = currentBit
        num = num >> 1
    }
    true
}
  def main(args: Array[String]): Unit = {
    assert(hasAlternatingBits(10)) // Binary representation of 10 is 1010 which has alternating bits
    assert(!hasAlternatingBits(7)) // Binary representation of 7 is 111 which does not have alternating bits
    assert(hasAlternatingBits(21)) // Binary representation of 21 is 10101 which has alternating bits
    
    println("All tests passed")
  }
}