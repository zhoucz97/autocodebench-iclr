

# Given an array of integers nums, return the minimum number of operations required to make all elements in the array equal.
# In one operation, you can either increment or decrement one element in the array.
# @param nums An array of integers.
# @return The minimum number of operations required.
#
# Example 1:
# Input: nums = [1,2,3]
# Output: 2
#
# Example 2:
# Input: nums = [1,10,2,9]
# Output: 16
#
# Example 3:
# Input: nums = [5,5,5,5]
# Output: 0
minOperations = (nums) ->
  nums.sort (a, b) -> a - b
  median = nums[Math.floor(nums.length / 2)]
  operations = 0
  for num in nums
    operations += Math.abs(num - median)
  operations
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test cases
main = ->
  assertEqual(minOperations([3,1,10]), 9, "Example case 1 failed")
  assertEqual(minOperations([8,8,8,8,9]), 1, "Example case 2 failed")
  assertEqual(minOperations([0,0,3,4]), 7, "Example case 3 failed")
  console.log("All tests passed")

# Run the test cases    
main()