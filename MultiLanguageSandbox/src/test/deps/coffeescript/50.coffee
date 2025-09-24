

# Given an integer n, return the minimum number of perfect square numbers that sum up to n.
minPerfectSquareSum = (n) ->
  # Create a DP array where dp[i] will store the minimum number of perfect squares that sum to i
  dp = new Array(n + 1).fill(Infinity)
  
  # Base case: 0 requires 0 perfect squares
  dp[0] = 0
  
  # Fill the DP array
  for i in [1..n]
    j = 1
    while j * j <= i
      dp[i] = Math.min(dp[i], dp[i - j * j] + 1)
      j++
  
  # Return the result for n
  dp[n]

# Example usage:
# Custom AssertionError class for testing
class AssertionError extends Error

# Custom assert function for testing equality of actual and expected values
assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless JSON.stringify(actual) is JSON.stringify(expected) 


# Test cases using assert function
main = ->
  assertEqual minPerfectSquareSum(26), 2, "Test case 1 failed"
  assertEqual minPerfectSquareSum(17), 2, "Test case 2 failed"
  assertEqual minPerfectSquareSum(99), 3, "Test case 3 failed"
  console.log "All tests passed"

main()