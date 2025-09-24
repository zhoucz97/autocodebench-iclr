

# Given a string s, check if it can be divided into three non-empty palindrome substrings.
# @param s A string.
# @return True if s can be divided into three non-empty palindrome substrings, False otherwise.
#
# Example 1:
# Input: s = "abcbdd"
# Output: true
#
# Example 2:
# Input: s = "bcbddxy"
# Output: false
#
# Example 3:
# Input: s = "abcba"
# Output: false
#
# Example 4:
# Input: s = "aaa"
# Output: true
#
# Example 5:
# Input: s = "abcde"
# Output: false
isDivisibleIntoThreePalindromes = (s) ->
  n = s.length
  for i in [1..n-2]
    for j in [i+1..n-1]
      part1 = s[0...i]
      part2 = s[i...j]
      part3 = s[j...n]
      if isPalindrome(part1) and isPalindrome(part2) and isPalindrome(part3)
        return true
  false

isPalindrome = (str) ->
  str == str.split('').reverse().join('')
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  assertEqual(isDivisibleIntoThreePalindromes("aaabbbccc"), true, "Test 1 Failed")
  assertEqual(isDivisibleIntoThreePalindromes("madamracecar"), false, "Test 2 Failed")
  assertEqual(isDivisibleIntoThreePalindromes("abacaba"), true, "Test 3 Failed")
  console.log "All tests passed"

main()