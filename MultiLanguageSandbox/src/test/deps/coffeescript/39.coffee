

# Given a non-empty string s, check if it can be constructed by repeating a substring.
# @param s A non-empty string.
# @return True if s can be constructed by repeating a substring, False otherwise.
#
# Example 1:
# Input: s = "abab"
# Output: true
#
# Example 2:
# Input: s = "aba"
# Output: false
#
# Example 3:
# Input: s = "abcabcabcabc"
# Output: true
isRepeatedSubstring = (s) ->
  n = s.length
  for i in [1..n/2]
    if n % i == 0
      substring = s.substring(0, i)
      repeated = substring.repeat(n / i)
      if repeated == s
        return true
  false
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  # Test cases
  assertEqual(isRepeatedSubstring("abcdabcd"), true, "Failed on test case 1")
  assertEqual(isRepeatedSubstring("xyz"), false, "Failed on test case 2")
  assertEqual(isRepeatedSubstring("pppppppp"), true, "Failed on test case 3")

  console.log("All tests passed")

main()