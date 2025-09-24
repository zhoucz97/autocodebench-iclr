

# Determine if a sentence is a pangram.
# @param sentence A string consisting of lowercase letters.
# @return True if the sentence is a pangram, False otherwise.
#
# Example 1:
# Input: sentence = "thequickbrownfoxjumpsoverthelazydog"
# Output: true
#
# Example 2:
# Input: sentence = "leetcode"
# Output: false
#
isPangram = (sentence) ->
  # Create a set of all unique characters in the sentence
  chars = new Set(sentence)
  # Check if the size of the set is 26 (all letters of the alphabet)
  chars.size == 26
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test cases
main = ->
  assertEqual isPangram("thequickbrownfoxjumpsoverthelazydog"), true, "Test case 1 failed"
  assertEqual isPangram("leetcode"), true, "Test case 2 failed"
  assertEqual isPangram("abcdefghijklmnopqrstuvwxyz123"), false, "Test case 3 failed"
  console.log "All tests passed"

main()