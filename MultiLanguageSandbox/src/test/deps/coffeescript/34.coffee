

# Given an integer n, determine whether it is an Armstrong number.
# An Armstrong number is a number that is equal to the sum of its digits raised to the power of the number of digits.
# @param n An integer.
# @return True if n is an Armstrong number, False otherwise.
#
# Example 1:
# Input: n = 153
# Output: true
# Explanation: 153 is a 3-digit number and 153 = 1^3 + 5^3 + 3^3.
#
# Example 2:
# Input: n = 123
# Output: false
# Explanation: 123 is a 3-digit number but 123 != 1^3 + 2^3 + 3^3 = 36.
isArmstrongNumber = (n) ->
  digits = String(n).split('')
  numDigits = digits.length
  sum = 0
  for digit in digits
    sum += Math.pow(Number(digit), numDigits)
  sum == n
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message || "Expected #{actual} to equal #{expected}") unless actual is expected
main = ->
  assertEqual isArmstrongNumber(371), true, "Test case 1 failed"
  assertEqual isArmstrongNumber(9474), true, "Test case 2 failed"
  assertEqual isArmstrongNumber(9475), false, "Test case 3 failed"
  console.log("All tests passed")

main()