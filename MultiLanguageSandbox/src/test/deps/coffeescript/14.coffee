

# Given a string s, find the index of the first non-repeating character and return it. If it does not exist, return -1.
# @param s A string.
# @return The index of the first non-repeating character, or -1 if it does not exist.
#
# Example 1:
# Input: s = "leetcode"
# Output: 0
#
# Example 2:
# Input: s = "loveleetcode"
# Output: 2
#
# Example 3:
# Input: s = "aabb"
# Output: -1
#
# Constraints:
# 1 <= s.length <= 105
# s only contains lowercase letters
firstUniqChar = (s) ->
  # Create a frequency map to count occurrences of each character
  frequency = {}
  
  # First pass: count the frequency of each character
  for char in s
    frequency[char] ?= 0
    frequency[char] += 1
  
  # Second pass: find the first character with frequency 1
  for char, index in s
    if frequency[char] == 1
      return index
  
  # If no unique character found, return -1
  -1
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test cases
main = ->
  assertEqual(firstUniqChar("hello"), 0, "Test case 1 failed")
  assertEqual(firstUniqChar("goodbye"), 0, "Test case 2 failed")
  assertEqual(firstUniqChar("abcabc"), -1, "Test case 3 failed")

  console.log("All tests passed")

main()