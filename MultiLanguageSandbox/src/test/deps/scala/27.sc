

object Solution {
    /**
     * Remove the trailing zeros from a given integer represented as a string.
     *
     * @param num The string representation of the integer.
     * @return The integer with trailing zeros removed represented as a string.
     *
     * Example:
     * removeTrailingZeros("51230100")
     * Output: "512301"
     *
     * removeTrailingZeros("123")
     * Output: "123"
     */
  def removeTrailingZeros(num: String): String = {
  if (num.isEmpty) num
  else {
    val trimmed = num.reverse.dropWhile(_ == '0').reverse
    if (trimmed.isEmpty) "0" else trimmed
  }
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(removeTrailingZeros("51230100") == "512301")
    assert(removeTrailingZeros("123") == "123")
    assert(removeTrailingZeros("1000010") == "100001")

    println("All tests passed")
  }
}