


object Solution {
      /**
       * Reverse the first k characters of every 2k characters in the given string.
       *
       * @param s The input string.
       * @param k The integer k.
       * @return The modified string.
       *
       * Example:
       * reverseString("abcdefg", 2)
       * Output: "bacdfeg"
       *
       * reverseString("abcd", 2)
       * Output: "bacd"
       */
  def reverseString(s: String, k: Int): String = {
  val chars = s.toCharArray
  for (i <- 0 until chars.length by 2 * k) {
    val start = i
    val end = math.min(i + k - 1, chars.length - 1)
    var left = start
    var right = end
    while (left < right) {
      val temp = chars(left)
      chars(left) = chars(right)
      chars(right) = temp
      left += 1
      right -= 1
    }
  }
  new String(chars)
}
  def main(args: Array[String]): Unit = {
    assert(reverseString("example", 3) == "axemple")
    assert(reverseString("helloScala", 4) == "llehoScaal")
    assert(reverseString("scalaisfun", 5) == "alacsisfun")

    println("All tests passed")
  }
}