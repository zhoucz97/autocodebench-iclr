

# Given a non-empty integer array nums, every element appears twice except for one. Find that single one.
# You must implement an algorithm with linear runtime complexity and use only constant extra space.
#
# @param nums A non-empty integer array.
# @return The single number that appears only once.
#
# Example 1:
# Input: nums = [2,2,1]
# Output: 1
#
# Example 2:
# Input: nums = [4,1,2,1,2]
# Output: 4
#
# Example 3:
# Input: nums = [1]
# Output: 1

singleNumber = (nums) ->
  result = 0
  for num in nums
    result ^= num
  result
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test Cases
main = ->
  # Generate Test Cases
  nums1 = [3, 3, 7, 7, 9]
  expected1 = 9

  nums2 = [5, 2, 5, 9, 2]
  expected2 = 9

  nums3 = [1, 4, 1, 4, 6]
  expected3 = 6

  # Run Test Cases
  assertEqual(singleNumber(nums1), expected1, "Test Case 1 Failed")
  assertEqual(singleNumber(nums2), expected2, "Test Case 2 Failed")
  assertEqual(singleNumber(nums3), expected3, "Test Case 3 Failed")

  console.log "All tests passed"

main()