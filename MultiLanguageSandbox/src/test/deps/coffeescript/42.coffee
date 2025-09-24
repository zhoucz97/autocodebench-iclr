

# Given an integer n, return the number of unique binary search trees that can be formed using n nodes.
# The nodes are labeled from 1 to n.
# @param n An integer representing the number of nodes.
# @return The number of unique binary search trees.
#
# Example 1:
# Input: n = 3
# Output: 5
#
# Example 2:
# Input: n = 1
# Output: 1
numTrees = (n) ->
  dp = new Array(n + 1).fill(0)
  dp[0] = 1
  for i in [1..n]
    for j in [1..i]
      dp[i] += dp[j - 1] * dp[i - j]
  dp[n]
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Testing the numTrees function with 3 different test cases
main = ->
  assertEqual(numTrees(3), 5, "Test case 1 failed")
  assertEqual(numTrees(1), 1, "Test case 2 failed")
  assertEqual(numTrees(4), 14, "Test case 3 failed")
  console.log "All tests passed"

main()