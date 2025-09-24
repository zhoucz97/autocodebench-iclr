

# Given a non-negative integer c, determine if there are two integers a and b such that a^2 + b^2 = c.
# @param c A non-negative integer.
# @return A boolean value indicating whether there are two integers a and b such that a^2 + b^2 = c.
#
# Example 1:
# Input: c = 5
# Output: true
#
# Example 2:
# Input: c = 3
# Output: false
#
# Constraints:
# 0 <= c <= 231 - 1
isSumOfSquares = (c) ->
  # Handle edge cases where c is 0 or 1
  return true if c == 0 or c == 1
  
  # Iterate through possible values of a
  for a in [0..Math.sqrt(c)]
    bSquared = c - a * a
    b = Math.sqrt(bSquared)
    # Check if b is an integer
    if b == Math.floor(b)
      return true
  false
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test cases
main = ->
  assertEqual(isSumOfSquares(10), true, "10 is a sum of two squares")
  assertEqual(isSumOfSquares(11), false, "8 is not a sum of two squares")
  assertEqual(isSumOfSquares(50), true, "50 is a sum of two squares")
  console.log "All tests passed"

# Run the test cases
main()  