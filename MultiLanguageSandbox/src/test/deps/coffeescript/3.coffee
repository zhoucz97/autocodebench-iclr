

# Given a string s, convert all uppercase letters to lowercase letters and return the new string.
# @param s A string.
# @return The new converted string.
#
# Example 1:
# Input: s = "Hello"
# Output: "hello"
#
# Example 2:
# Input: s = "here"
# Output: "here"
#
# Example 3:
# Input: s = "LOVELY"
# Output: "lovely"
toLowerCase = (s) ->
  s.toLowerCase()
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  # Test Case 1
  s = "Hello"
  expected = "hello"
  assertEqual(toLowerCase(s), expected, "Test Case 1 Failed")

  # Test Case 2
  s = "here"
  expected = "here"
  assertEqual(toLowerCase(s), expected, "Test Case 2 Failed")

  # Test Case 3
  s = "LOVELY"
  expected = "lovely"
  assertEqual(toLowerCase(s), expected, "Test Case 3 Failed")

  console.log("All tests passed")

main()