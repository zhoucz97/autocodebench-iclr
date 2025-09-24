

# Given an array of integers nums, return the number of longest increasing subsequences.
# The subsequence must be strictly increasing.
#
# @param nums An array of integers.
# @return The number of longest increasing subsequences.
#
# Example 1:
# Input: [1,3,5,4,7]
# Output: 2
# Explanation: There are two longest increasing subsequences: [1, 3, 4, 7] and [1, 3, 5, 7].
#
# Example 2:
# Input: [2,2,2,2,2]
# Output: 5
# Explanation: The length of the longest increasing subsequence is 1, and there are 5 subsequences with length 1, so the output is 5.
#
# Constraints:
# 1 <= nums.length <= 2000
# -10^6 <= nums[i] <= 10^6
countLongestIncreasingSubsequences = (nums) ->
  n = nums.length
  return 0 if n == 0
  
  lengths = new Array(n).fill(1)
  counts = new Array(n).fill(1)
  
  for i in [0...n]
    for j in [0...i]
      if nums[j] < nums[i]
        if lengths[j] + 1 > lengths[i]
          lengths[i] = lengths[j] + 1
          counts[i] = counts[j]
        else if lengths[j] + 1 == lengths[i]
          counts[i] += counts[j]
  
  max_length = Math.max(...lengths)
  result = 0
  for i in [0...n]
    if lengths[i] == max_length
      result += counts[i]
  
  result
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected

# Test cases
main = ->
  nums1 = [1, 3, 5, 4, 7]
  assertEqual countLongestIncreasingSubsequences(nums1), 2, "Test case 1 failed"

  nums2 = [2, 2, 2, 2, 2]
  assertEqual countLongestIncreasingSubsequences(nums2), 5, "Test case 2 failed"

  nums3 = [3, 1, 2, 4, 6, 5, 8]
  assertEqual countLongestIncreasingSubsequences(nums3), 2, "Test case 3 failed"
  console.log "All tests passed"

main()