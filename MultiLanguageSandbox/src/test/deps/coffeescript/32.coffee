

# Given an array of integers nums and an integer k, return the kth largest element in the array.
# Note: You need to find the kth largest element after sorting the array, not the kth distinct element.
# You must design and implement an algorithm with O(n) time complexity to solve this problem.
#
# @param nums An array of integers.
# @param k The value of k.
# @return The kth largest element in the array.
#
# Example 1:
# Input: nums = [3,2,1,5,6,4], k = 2
# Output: 5
#
# Example 2:
# Input: nums = [3,2,3,1,2,4,5,5,6], k = 4
# Output: 4
#
# Constraints:
# 1 <= k <= nums.length <= 105
# -104 <= nums[i] <= 104
findKthLargest = (nums, k) ->
  # Convert k to the index in the sorted array (0-based)
  kIndex = nums.length - k
  
  quickSelect = (left, right) ->
    if left == right
      return nums[left]
    
    pivotIndex = partition(left, right)
    
    if pivotIndex == kIndex
      return nums[pivotIndex]
    else if pivotIndex < kIndex
      return quickSelect(pivotIndex + 1, right)
    else
      return quickSelect(left, pivotIndex - 1)
  
  partition = (left, right) ->
    pivot = nums[right]
    i = left
    for j in [left...right]
      if nums[j] > pivot
        [nums[i], nums[j]] = [nums[j], nums[i]]
        i++
    [nums[i], nums[right]] = [nums[right], nums[i]]
    return i
  
  quickSelect(0, nums.length - 1)
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless JSON.stringify(actual) is JSON.stringify(expected) 
main = ->
  # First Test Case
  assertEqual findKthLargest([7, 10, 4, 3, 20, 15], 3), 10
  # Second Test Case
  assertEqual findKthLargest([1, 2, 3, 4, 5, 6], 4), 3
  # Third Test Case
  assertEqual findKthLargest([100, 36, 1, 3, 2, 25, 9], 2), 36
  console.log 'All tests passed'

main()