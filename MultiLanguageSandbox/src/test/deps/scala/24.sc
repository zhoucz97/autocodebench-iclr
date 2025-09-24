

object Solution {
    /**
     * Calculate the sum of the digits in a number with assigned signs.
     *
     * @param n The input number.
     * @return The sum of the digits with assigned signs.
     *
     * Example:
     * signSum(521)
     * Output: 4
     */
  def signSum(n: Int): Int = {
    val digits = n.toString.map(_.asDigit)
    var sum = 0
    var sign = 1 // starts with positive
    
    for (digit <- digits) {
        sum += sign * digit
        sign *= -1 // toggle the sign for the next digit
    }
    
    sum
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(signSum(123) == 6)
    assert(signSum(-456) == -15)
    assert(signSum(789) == 24)

    println("All tests passed")
  }
}