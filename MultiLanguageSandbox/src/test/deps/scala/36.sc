


object Solution {
      /**
       * Count the number of palindrome substrings in a given string.
       *
       * @param s The input string.
       * @return The number of palindrome substrings.
       *
       * Example:
       * countPalindromeSubstrings("abc")
       * Output: 3
       *
       * Example:
       * countPalindromeSubstrings("aaa")
       * Output: 6
       */
  def countPalindromeSubstrings(s: String): Int = {
    var count = 0
    val n = s.length
    
    for (i <- 0 until n) {
        // Odd length palindromes
        var left = i
        var right = i
        while (left >= 0 && right < n && s(left) == s(right)) {
            count += 1
            left -= 1
            right += 1
        }
        
        // Even length palindromes
        left = i
        right = i + 1
        while (left >= 0 && right < n && s(left) == s(right)) {
            count += 1
            left -= 1
            right += 1
        }
    }
    
    count
}
  def main(args: Array[String]): Unit = {
    assert(countPalindromeSubstrings("abc") == 3)
    assert(countPalindromeSubstrings("aaa") == 6)
    assert(countPalindromeSubstrings("abba") == 6)
    assert(countPalindromeSubstrings("abcd") == 4)
    assert(countPalindromeSubstrings("aabbccd") == 10)
    println("All tests passed")
  }
}