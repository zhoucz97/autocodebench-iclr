

# Given an array of positive integers A,
# calculate the sum of the digits in the smallest element S.
# If the sum is odd, return 0; if it is even, return 1.
#
# @param A An array of positive integers.
# @return 0 if the sum of digits is odd, 1 if it is even.
#
# Example 1:
# Input: [34,23,1,24,75,33,54,8]
# Output: 0
#
# Example 2:
# Input: [99,77,33,66,55]
# Output: 1
smallestDigitSum = (A) ->
  # Find the smallest element in the array
  S = Math.min(A...)
  
  # Calculate the sum of its digits
  sum = 0
  while S > 0
    sum += S % 10
    S = Math.floor(S / 10)
  
  # Return 0 if sum is odd, 1 if even
  if sum % 2 == 0 then 1 else 0
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test cases
main = ->
  assertEqual(smallestDigitSum([12, 34, 56, 78]), 0, "Test case 1")
  assertEqual(smallestDigitSum([9, 8, 7, 6, 5]), 0, "Test case 2")
  assertEqual(smallestDigitSum([123, 456, 789]), 1, "Test case 3")
  console.log("All tests passed")

main()