

# Given a string s and an integer k, find the length of the longest substring that contains at most k distinct characters.
# @param s A string.
# @param k The maximum number of distinct characters allowed in the substring.
# @return The length of the longest substring.
#
# Example 1:
# Input: s = "eceba", k = 2
# Output: 3
# Explanation: The longest substring with at most 2 distinct characters is "ece" with a length of 3.
#
# Example 2:
# Input: s = "aa", k = 1
# Output: 2
# Explanation: The longest substring with at most 1 distinct character is "aa" with a length of 2.
longestSubstringLength = (s, k) ->
  return 0 if s.length == 0 or k == 0

  left = 0
  maxLen = 0
  charCount = {}
  distinctChars = 0

  for right in [0...s.length]
    char = s[right]
    
    # If the character is not in the map, add it
    unless char of charCount
      charCount[char] = 0
      distinctChars++
    
    # Increment the count for the current character
    charCount[char]++
    
    # If we have more than k distinct characters, move the left pointer
    while distinctChars > k
      leftChar = s[left]
      charCount[leftChar]--
      
      if charCount[leftChar] == 0
        delete charCount[leftChar]
        distinctChars--
      
      left++
    
    # Update the maximum length found so far
    currentLen = right - left + 1
    maxLen = Math.max(maxLen, currentLen)
  
  maxLen
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test Cases
try
  assertEqual longestSubstringLength("abcabcbb", 2), 4, "Test Case 1 Failed"
  assertEqual longestSubstringLength("abaccc", 2), 4, "Test Case 2 Failed"
  assertEqual longestSubstringLength("aabbcc", 3), 6, "Test Case 3 Failed"

  console.log "All tests passed"
catch error
  console.error error