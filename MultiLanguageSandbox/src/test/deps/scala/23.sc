

object Solution {
    /**
     * Find the smallest positive integer b such that the product of all digits of b is equal to a.
     *
     * @param a The given integer.
     * @return The smallest integer b whose product of digits is equal to a. If no such number exists or the result
     *         is not a 32-bit signed integer, return 0.
     *
     * Example:
     * findSmallestInteger(48)
     * Output: 68
     *
     * Example:
     * findSmallestInteger(15)
     * Output: 35
     */
  def findSmallestInteger(a: Int): Int = {
    if (a == 0) {
        // The smallest number with a digit product of 0 is 10 (1*0=0)
        // But the problem might expect 0 if no such number exists. Need to clarify.
        // Assuming the problem expects 0 for a=0 as per the note.
        0
    } else if (a == 1) {
        1
    } else {
        var remaining = a
        val digits = scala.collection.mutable.ArrayBuffer[Int]()
        
        // Factorize into digits 9 down to 2
        for (d <- 9 to 2 by -1) {
            while (remaining % d == 0) {
                digits += d
                remaining /= d
            }
        }
        
        if (remaining != 1) {
            // Cannot factorize completely into digits 2-9
            0
        } else {
            // Sort digits in ascending order to form the smallest number
            val sortedDigits = digits.sorted
            var b = 0L
            for (digit <- sortedDigits) {
                b = b * 10 + digit
                // Check if b exceeds Int.MaxValue
                if (b > Int.MaxValue) {
                    return 0
                }
            }
            b.toInt
        }
    }
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(findSmallestInteger(48) == 68)
    assert(findSmallestInteger(15) == 35)

    println("All tests passed")
  }
}