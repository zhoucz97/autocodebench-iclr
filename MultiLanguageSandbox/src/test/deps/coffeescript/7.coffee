

# Given two strings s and t, where t is formed by randomly shuffling the characters in s and adding one extra letter at a random position.
# Find the letter that was added in t.
# @param s The original string.
# @param t The shuffled string with one extra letter.
# @return The letter that was added in t.
#
# Example 1:
# Input: s = "abcd", t = "abcde"
# Output: "e"
#
# Example 2:
# Input: s = "", t = "y"
# Output: "y"
#
# Constraints:
# 0 <= s.length <= 1000
# t.length == s.length + 1
# s and t contain only lowercase letters.
findAddedLetter = (s, t) ->
  # Create a frequency map for characters in s
  freq = {}
  for char in s
    freq[char] ?= 0
    freq[char] += 1

  # Iterate through t to find the extra character
  for char in t
    if freq[char]?
      freq[char] -= 1
      if freq[char] < 0
        return char
    else
      return char
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Testing
main = ->
  # Test Case 1
  s = "abcd"
  t = "abcde"
  addedLetter = findAddedLetter(s, t)
  assertEqual(addedLetter, "e", "Test Case 1 Failed")

  # Test Case 2
  s = ""
  t = "y"
  addedLetter = findAddedLetter(s, t)
  assertEqual(addedLetter, "y", "Test Case 2 Failed")

  # Test Case 3
  s = "hello"
  t = "hlello"
  addedLetter = findAddedLetter(s, t)
  assertEqual(addedLetter, "l", "Test Case 3 Failed")

  console.log("All tests passed")

main()