

# Given two integers num1 and num2, return the sum of the two numbers.
# @param num1 An integer.
# @param num2 An integer.
# @return The sum of num1 and num2.
#
# Example 1:
# Input: num1 = 12, num2 = 5
# Output: 17
#
# Example 2:
# Input: num1 = -10, num2 = 4
# Output: -6
#
numSum = (num1, num2) ->
  num1 + num2
class AssertionError extends Error
assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# 测试用例
main = ->
  try
    assertEqual numSum(3, 4), 7, 'Test Case 1 Failed'
    assertEqual numSum(-5, 10), 5, 'Test Case 2 Failed'
    assertEqual numSum(0, 0), 0, 'Test Case 3 Failed'
    console.log 'All tests passed'
  catch error
    console.error error

main()