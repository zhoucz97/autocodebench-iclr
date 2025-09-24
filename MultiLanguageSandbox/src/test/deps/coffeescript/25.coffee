

# Given an integer num, return three consecutive integers that add up to num. If num cannot be represented as the sum of three consecutive integers, return an empty array.
# @param num An integer.
# @return Three consecutive integers that add up to num.
#
# Example 1:
# Input: num = 33
# Output: [10,11,12]
# Explanation: 33 can be represented as 10 + 11 + 12 = 33. So, return [10, 11, 12] which are three consecutive integers.
#
# Example 2:
# Input: num = 4
# Output: []
# Explanation: There is no way to represent 4 as the sum of three consecutive integers. So, return an empty array.
consecutiveSum = (num) ->
  # The sum of three consecutive integers x, x+1, x+2 is 3x + 3
  # So we need 3x + 3 = num => x = (num - 3) / 3
  # x must be an integer for the solution to exist
  x = (num - 3) / 3
  if x % 1 == 0
    [x, x + 1, x + 2]
  else
    []
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  result = consecutiveSum(33)
  assertEqual result.length, 3, "Test Case 1 failed"
  assertEqual result[0], 10, "Test Case 1 failed"
  assertEqual result[1], 11, "Test Case 1 failed"
  assertEqual result[2], 12, "Test Case 1 failed"

  result = consecutiveSum(4)
  assertEqual result.length, 0, "Test Case 2 failed"

  result = consecutiveSum(21)
  assertEqual result.length, 3, "Test Case 3 failed"
  assertEqual result[0], 6, "Test Case 3 failed"
  assertEqual result[1], 7, "Test Case 3 failed"
  assertEqual result[2], 8, "Test Case 3 failed"

  console.log "All tests passed"

main()