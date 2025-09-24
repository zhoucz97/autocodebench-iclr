

object Solution {
    /**
    * Check if a string is a k-palindrome.
    *
    * A k-palindrome is a string that can be converted into a palindrome by removing at most k characters.
    *
    * @param s The input string.
    * @param k The maximum number of characters that can be removed.
    * @return True if the string is a k-palindrome, otherwise false.
    *
    * Example:
    * isKPalindrome("abcdeca", 2)
    * Output: true
    *
    * isKPalindrome("abbababa", 1)
    * Output: true
    */
  def isKPalindrome(s: String, k: Int): Boolean = {
    val n = s.length
    if (n <= 1) return true
    
    val dp = Array.ofDim[Int](n, n)
    
    for (i <- 0 until n) {
        dp(i)(i) = 1
    }
    
    for (length <- 2 to n) {
        for (i <- 0 until n - length + 1) {
            val j = i + length - 1
            if (s(i) == s(j)) {
                if (length == 2) {
                    dp(i)(j) = 2
                } else {
                    dp(i)(j) = 2 + dp(i + 1)(j - 1)
                }
            } else {
                dp(i)(j) = math.max(dp(i + 1)(j), dp(i)(j - 1))
            }
        }
    }
    
    val lpsLength = dp(0)(n - 1)
    n - lpsLength <= k
}
  def main(args: Array[String]): Unit = {
    // Test cases
    assert(isKPalindrome("abcdeca", 2) == true)
    assert(isKPalindrome("abbababa", 1) == true)
    assert(isKPalindrome("abcdefg", 3) == false)

    println("All tests passed")
  }
}