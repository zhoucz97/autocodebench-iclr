

# Reverse the given string in-place.
# @param s An array of characters representing the string.
# @return The reversed string.
#
# Example 1:
# Input: s = ["h","e","l","l","o"]
# Output: ["o","l","l","e","h"]
#
# Example 2:
# Input: s = ["H","a","n","n","a","h"]
# Output: ["h","a","n","n","a","H"]
reverseString = (s) ->
  left = 0
  right = s.length - 1
  while left < right
    # Swap characters at left and right indices
    temp = s[left]
    s[left] = s[right]
    s[right] = temp
    left++
    right--
  s
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless JSON.stringify(actual) is JSON.stringify(expected) 
main = ->
  # Test 1
  testString1 = ["C","o","f","f","e","e"]
  expected1 = ["e","e","f","f","o","C"]
  assertEqual(reverseString(testString1), expected1, "Test 1 Failed")

  # Test 2
  testString2 = ["J","a","v","a","S","c","r","i","p","t"]
  expected2 = ["t","p","i","r","c","S","a","v","a","J"]
  assertEqual(reverseString(testString2), expected2, "Test 2 Failed")

  # Test 3
  testString3 = ["P","y","t","h","o","n"]
  expected3 = ["n","o","h","t","y","P"]
  assertEqual(reverseString(testString3), expected3, "Test 3 Failed")

  console.log("All tests passed")

main()