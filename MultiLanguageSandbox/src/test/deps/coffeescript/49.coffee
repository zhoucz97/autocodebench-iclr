

# Given a non-negative integer num, repeatedly add all its digits until
# the result is a single digit. Return the result.
#
# @param num The non-negative integer to perform the operation on.
# @return The single digit result after repeatedly adding all digits.
#
# Example:
# addDigits(38)
# Output: 2

addDigits = (num) ->
  while num >= 10
    sum = 0
    while num > 0
      sum += num % 10
      num = Math.floor(num / 10)
    num = sum
  num
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) if JSON.stringify(actual) isnt JSON.stringify(expected)

# Test cases using assert function
main = ->
  assertEqual(addDigits(123), 6, "Test case 1 failed")
  assertEqual(addDigits(59), 5, "Test case 2 failed")
  assertEqual(addDigits(0), 0, "Test case 3 failed")
  console.log("All tests passed")


main()