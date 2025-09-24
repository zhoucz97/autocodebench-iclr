

# Given two non-negative integers low and high, return the count of odd numbers between low and high (inclusive).
# @param low A non-negative integer representing the lower bound.
# @param high A non-negative integer representing the upper bound.
# @return The count of odd numbers between low and high.
#
# Example 1:
# Input: low = 3, high = 7
# Output: 3
# Explanation: The odd numbers between 3 and 7 are [3, 5, 7].
#
# Example 2:
# Input: low = 8, high = 10
# Output: 1
# Explanation: The odd number between 8 and 10 is [9].
countOddNumbers = (low, high) ->
  # Calculate the total numbers in the range
  totalNumbers = high - low + 1
  # If either low or high is odd, there's one more odd number
  if low % 2 == 1 or high % 2 == 1
    Math.ceil(totalNumbers / 2)
  else
    totalNumbers / 2
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  assertEqual countOddNumbers(1, 5), 3, "Test Case 1 Failed"
  assertEqual countOddNumbers(10, 20), 5, "Test Case 2 Failed"
  assertEqual countOddNumbers(15, 15), 1, "Test Case 3 Failed"
  console.log "All tests passed"

main()