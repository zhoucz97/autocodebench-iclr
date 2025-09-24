

# Given a signed 32-bit integer x, return the reverse of x.
# If reversing x causes the value to go outside the signed 32-bit integer range [-2^31, 2^31 - 1], then return 0.
# Assume the environment does not allow storing 64-bit integers (signed or unsigned).

# @param x A signed 32-bit integer.
# @return The reverse of x if it is within the 32-bit integer range, or 0 if it is outside the range.
#
# Example 1:
# Input: x = 123
# Output: 321
#
# Example 2:
# Input: x = -123
# Output: -321
#
# Example 3:
# Input: x = 120
# Output: 21
#
# Example 4:
# Input: x = 0
# Output: 0
reverseInteger = (x) ->
  # Handle negative numbers by storing the sign and working with absolute value
  sign = if x < 0 then -1 else 1
  x = Math.abs(x)
  
  reversed = 0
  max32Bit = Math.pow(2, 31) - 1  # 2^31 - 1
  min32Bit = -Math.pow(2, 31)     # -2^31
  
  while x > 0
    digit = x % 10
    reversed = reversed * 10 + digit
    x = Math.floor(x / 10)
    
    # Check for overflow before multiplying by 10 in the next iteration
    if reversed > max32Bit / 10 or (reversed == max32Bit / 10 and digit > 7)
      return 0
    if reversed < min32Bit / 10 or (reversed == min32Bit / 10 and digit < -8)
      return 0
  
  reversed * sign
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected

main = ->
  # Test cases
  assertEqual(reverseInteger(123), 321, "Test Case 1 Failed")
  assertEqual(reverseInteger(-123), -321, "Test Case 2 Failed")
  assertEqual(reverseInteger(120), 21, "Test Case 3 Failed")

  console.log("All tests passed")

main()