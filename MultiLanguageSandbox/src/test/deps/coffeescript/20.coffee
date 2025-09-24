

# Given an integer array A, find and return the maximum integer that appears only once in the array.
# If there is no integer that appears only once, return -1.
#
# @param A An array of integers.
# @return The maximum integer that appears only once, or -1 if none exists.
#
# Example 1:
# Input: [5,7,3,9,4,9,8,3,1]
# Output: 8
#
# Example 2:
# Input: [9,9,8,8]
# Output: -1
#
# Note:
# - The length of the array A is 1 to 2000.
# - The elements of A are integers between 0 and 1000.
findMaxUniqueInteger = (A) ->
  frequency = {}
  
  # Count the frequency of each number in the array
  for num in A
    frequency[num] ?= 0
    frequency[num] += 1
  
  maxUnique = -1
  
  # Iterate through the frequency map to find the maximum unique number
  for num, count of frequency
    if count == 1 and num > maxUnique
      maxUnique = num
  
  maxUnique
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected

# Testing the algorithm
main = ->
  # Test case 1
  arr1 = [5, 7, 3, 9, 4, 9, 8, 3, 1]
  assertEqual(findMaxUniqueInteger(arr1), 8, "Test case 1 failed")

  # Test case 2
  arr2 = [9, 9, 8, 8]
  assertEqual(findMaxUniqueInteger(arr2), -1, "Test case 2 failed")

  # Test case 3
  arr3 = [2, 2, 3, 3, 4, 4, 5, 5]
  assertEqual(findMaxUniqueInteger(arr3), -1, "Test case 3 failed")

  console.log("All tests passed")

main()