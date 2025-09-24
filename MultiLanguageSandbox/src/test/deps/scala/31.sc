


object Solution {
  /**
  * Reverse the given string based on the following rules:
  * - Keep all non-alphabetic characters in their original positions.
  * - Reverse the positions of all alphabetic characters (both lowercase and uppercase).
  *
  * @param s The input string.
  * @return The reversed string.
  *
  * Example:
  * reverseString("ab-cd")
  * Output: "dc-ba"
  */
  def reverseString(s: String): String = {
  // Extract all alphabetic characters and reverse them
  val reversedAlphas = s.filter(_.isLetter).reverse
  
  // Rebuild the string, replacing alphabetic characters with the reversed ones
  var alphaIndex = 0
  s.map { c =>
    if (c.isLetter) {
      val reversedChar = reversedAlphas(alphaIndex)
      alphaIndex += 1
      reversedChar
    } else {
      c
    }
  }
}
  def main(args: Array[String]): Unit = {
    assert(reverseString("ab-cd") == "dc-ba")
    assert(reverseString("a-bC-dEf-ghIj") == "j-Ih-gfE-dCba")

    println("All tests passed")
  }
}