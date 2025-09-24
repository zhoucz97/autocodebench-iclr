

# Given two integers a and b, calculate the sum of the two integers without using the + or - operator.
# @param a An integer.
# @param b An integer.
# @return The sum of the two integers.
#
# Example 1:
# Input: a = 1, b = 2
# Output: 3
#
# Example 2:
# Input: a = 2, b = 3
# Output: 5
getSum = (a, b) ->
  while b != 0
    carry = a & b  # Calculate the carry
    a = a ^ b      # Sum of bits without considering the carry
    b = carry << 1 # Shift the carry to the left by 1 bit
  a

# Example usage:
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  # Test cases
  assertEqual(getSum(2, 5), 7, "Test Case 1 Failed")
  assertEqual(getSum(-1, 1), 0, "Test Case 2 Failed")
  assertEqual(getSum(10, -5), 5, "Test Case 3 Failed")

  console.log("All tests passed")

main()