

# Given two words word1 and word2, return the minimum number of steps required to make word1 and word2 identical.
# Each step can consist of deleting any character from either string.
#
# @param word1 The first word.
# @param word2 The second word.
# @return The minimum number of steps required.
#
# Example 1:
# Input: word1 = "sea", word2 = "eat"
# Output: 2
# Explanation: The first step is to transform "sea" into "ea", then the second step is to transform "eat" into "ea".
#
# Example 2:
# Input: word1 = "leetcode", word2 = "etco"
# Output: 4
#
# Constraints:
# 1 <= word1.length, word2.length <= 500
# word1 and word2 only contain lowercase English letters.
minSteps = (word1, word2) ->
  m = word1.length
  n = word2.length
  # Create a DP table initialized to 0
  dp = new Array(m + 1)
  for i in [0..m]
    dp[i] = new Array(n + 1).fill(0)
  
  # Fill the DP table
  for i in [1..m]
    for j in [1..n]
      if word1[i-1] == word2[j-1]
        dp[i][j] = dp[i-1][j-1] + 1
      else
        dp[i][j] = Math.max(dp[i-1][j], dp[i][j-1])
  
  lcsLength = dp[m][n]
  (m - lcsLength) + (n - lcsLength)
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Testing
main = ->
  assertEqual(minSteps("abc", "def"), 6, "Test Case 1 Failed")
  assertEqual(minSteps("hello", "world"), 8, "Test Case 2 Failed")
  assertEqual(minSteps("algorithm", "logarithm"), 4, "Test Case 3 Failed")

  console.log("All tests passed")

main()