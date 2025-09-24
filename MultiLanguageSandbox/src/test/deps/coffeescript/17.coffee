

# Given two strings, s and goal. Return true if s can be transformed into goal after several rotations.
# A rotation operation on s moves the leftmost character of s to the rightmost position.
# For example, if s = 'abcde', after one rotation, it becomes 'bcdea'.

# @param s The original string.
# @param goal The target string.
# @return True if s can be transformed into goal after several rotations, False otherwise.

# Example 1:
# Input: s = "abcde", goal = "cdeab"
# Output: true

# Example 2:
# Input: s = "abcde", goal = "abced"
# Output: false

# Constraints:
# 1 <= len(s), len(goal) <= 100
# s and goal consist of lowercase English letters.
isTransformable = (s, goal) ->
  return false if s.length != goal.length
  doubleS = s + s
  doubleS.includes(goal)
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test cases
main = ->
  # Test case 1
  s = "abcdef"
  goal = "defabc"
  assertEqual(isTransformable(s, goal), true, "Test case 1 failed")

  # Test case 2
  s = "abcde"
  goal = "edcba"
  assertEqual(isTransformable(s, goal), false, "Test case 2 failed")

  # Test case 3
  s = "xyz"
  goal = "zxy"
  assertEqual(isTransformable(s, goal), true, "Test case 3 failed")

  console.log("All tests passed")

main()