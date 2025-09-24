

# Determine whether a given number is an ugly number.
# An ugly number is a positive integer that is divisible by either 2, 3, or 5.
# @param n The number to be checked.
# @return True if the number is an ugly number, false otherwise.
#
# Example 1:
# Input: n = 6
# Output: true
#
# Example 2:
# Input: n = 1
# Output: true
#
# Example 3:
# Input: n = 14
# Output: false
isUgly = (n) ->
  return false if n <= 0
  factors = [2, 3, 5]
  for factor in factors
    while n % factor == 0
      n /= factor
  n == 1
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test Cases
main = ->
  assertEqual(isUgly(10), true, "Test Case 1 Failed")
  assertEqual(isUgly(14), false, "Test Case 2 Failed")
  assertEqual(isUgly(30), true, "Test Case 3 Failed")

  console.log("All tests passed")

main()