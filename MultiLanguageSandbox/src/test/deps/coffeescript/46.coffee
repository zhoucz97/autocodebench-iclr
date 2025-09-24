

#  Given an integer array nums where every element appears three times except for one, which appears only once. Find and return the element that appears only once.

#  @param nums An integer array where every element appears three times except for one.
#  @return The element that appears only once.

#  Example:
#  singleNumber(new int[]{2,2,3,2})
#  Output: 3

#  Example:
#  singleNumber(new int[]{0,1,0,1,0,1,99})
#  Output: 99
singleNumber = (nums) ->
  result = 0
  for i in [0...32]
    sum = 0
    for num in nums
      # Right shift num by i and check the least significant bit
      sum += (num >> i) & 1
    # If the sum is not divisible by 3, set the bit in result
    if sum % 3 != 0
      # For negative numbers, we need to handle the sign bit properly
      if i == 31
        result -= (1 << i)
      else
        result |= (1 << i)
  result
# Define the custom AssertionError class
class AssertionError extends Error

# Define the assert equal function
assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless JSON.stringify(actual) is JSON.stringify(expected)
# Testing the function using assert
main = ->
  assertEqual singleNumber([6, 1, 6, 6]), 1, "Test case 1 failed"
  assertEqual singleNumber([22, 22, 22, 11, 11, 11, 55]), 55, "Test case 2 failed"
  assertEqual singleNumber([-1, 3, -1, 3, -1, 3, 99]), 99, "Test case 3 failed"
  console.log "All tests passed"

# Run the tests
main()