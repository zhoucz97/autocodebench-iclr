

# Given two integer arrays nums1 and nums2, return the length of the longest common subarray between them.
# @param nums1 An array of integers.
# @param nums2 An array of integers.
# @return The length of the longest common subarray.
#
# Example 1:
# Input: nums1 = [1,2,3,2,1], nums2 = [3,2,1,4,7]
# Output: 3
#
# Example 2:
# Input: nums1 = [0,0,0,0,0], nums2 = [0,0,0,0,0]
# Output: 5
#
# Constraints:
# - 1 <= nums1.length, nums2.length <= 1000
# - 0 <= nums1[i], nums2[i] <= 100

findLength = (nums1, nums2) ->
  m = nums1.length
  n = nums2.length
  maxLen = 0
  
  # Create a DP table with (m+1) rows and (n+1) columns initialized to 0
  dp = new Array(m + 1)
  for i in [0..m]
    dp[i] = new Array(n + 1).fill(0)
  
  for i in [1..m]
    for j in [1..n]
      if nums1[i - 1] == nums2[j - 1]
        dp[i][j] = dp[i - 1][j - 1] + 1
        maxLen = Math.max(maxLen, dp[i][j])
  
  maxLen
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test Cases
main = ->
  nums1 = [1, 2, 3, 4, 5]
  nums2 = [5, 4, 3, 2, 1]
  assertEqual(findLength(nums1, nums2), 1, "Test Case 1 Failed")

  nums3 = [2, 4, 6, 8]
  nums4 = [1, 3, 5, 7]
  assertEqual(findLength(nums3, nums4), 0, "Test Case 2 Failed")

  nums5 = [1, 2, 3]
  nums6 = [4, 5, 6]
  assertEqual(findLength(nums5, nums6), 0, "Test Case 3 Failed")

  console.log("All tests passed")

main()