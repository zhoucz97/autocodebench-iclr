


object Solution {
  /**
     * Given a string s, you need to split it into some substrings so that each substring is a palindrome.
     * Return the minimum number of splits required.
     *
     * @param s The input string.
     * @return The minimum number of splits required.
     *
     * Example:
     * minCut("aab")
     * Output: 1
     *
     * Explanation: Only one split is required to separate s into ["aa", "b"], both of which are palindromes.
     *
     * Constraints:
     * - 1 <= s.length <= 2000
     * - s consists only of lowercase English letters.
     */
  def minCut(s: String): Int = {
        val n = s.length
        if (n == 0) return 0
        
        // Create a table to store whether s[i..j] is a palindrome
        val isPalindrome = Array.ofDim[Boolean](n, n)
        
        // Initialize the minCuts array
        val minCuts = Array.fill(n)(Int.MaxValue)
        
        for (i <- 0 until n) {
            var minCut = i  // Maximum cuts needed for s[0..i] is i (each character is a palindrome)
            for (j <- 0 to i) {
                // Check if s[j..i] is a palindrome
                if (s(j) == s(i) && (i - j <= 2 || isPalindrome(j + 1)(i - 1))) {
                    isPalindrome(j)(i) = true
                    if (j == 0) {
                        // The entire substring s[0..i] is a palindrome, no cut needed
                        minCut = 0
                    } else {
                        // The substring s[j..i] is a palindrome, so cuts before j plus 1
                        minCut = math.min(minCut, minCuts(j - 1) + 1)
                    }
                }
            }
            minCuts(i) = minCut
        }
        
        minCuts(n - 1)
    }

  def main(args: Array[String]): Unit = {
    // Test cases
    assert(minCut("abcba") == 0)
    assert(minCut("aabba") == 1)
    assert(minCut("abbababa") == 2)

    println("All tests passed")
  }
}