

# Given a non-negative integer, you can swap at most one digit. Return the maximum value you can get.
# @param num A non-negative integer.
# @return The maximum value after at most one digit swap.
#
# Example 1:
# Input: num = 2736
# Output: 7236
# Explanation: Swap the digit 2 and the digit 7.
#
# Example 2:
# Input: num = 9973
# Output: 9973
# Explanation: No swap is needed.
#
# Note:
# The given number is in the range [0, 10^8].
maximumSwap = (num) ->
  digits = String(num).split('').map(Number)
  n = digits.length
  return num if n == 1  # no swap possible for single-digit numbers

  # Create a last occurrence map for each digit
  lastOccurrence = {}
  for i in [0...n]
    lastOccurrence[digits[i]] = i

  # Iterate through each digit to find the best swap
  for i in [0...n]
    currentDigit = digits[i]
    # Check all digits greater than currentDigit from 9 down to currentDigit+1
    for d in [9..currentDigit+1]
      if lastOccurrence[d]? and lastOccurrence[d] > i
        # Swap the current digit with the last occurrence of d
        [digits[i], digits[lastOccurrence[d]]] = [digits[lastOccurrence[d]], digits[i]]
        return parseInt(digits.join(''), 10)

  # If no swap improves the number, return the original
  num
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected

# Testing
main = ->
  assertEqual(maximumSwap(2736), 7236, 'Test Case 1 Failed')
  assertEqual(maximumSwap(9973), 9973, 'Test Case 2 Failed')
  assertEqual(maximumSwap(1234), 4231, 'Test Case 3 Failed')

  console.log('All tests passed')

main()