

# Given two integers left and right, representing the range [left, right], return the bitwise AND of all the numbers in this range (including the endpoints).
# @param left An integer representing the left endpoint of the range.
# @param right An integer representing the right endpoint of the range.
# @return The bitwise AND of all the numbers in the range.
#
# Example 1:
# Input: left = 5, right = 7
# Output: 4
#
# Example 2:
# Input: left = 0, right = 0
# Output: 0
#
# Example 3:
# Input: left = 1, right = 2147483647
# Output: 0
bitwiseANDRange = (left, right) ->
  shift = 0
  # Right shift both left and right until they are equal
  while left != right
    left >>= 1
    right >>= 1
    shift += 1
  # Shift back to get the common prefix
  left << shift
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  assertEqual(bitwiseANDRange(10, 15), 8, "Test case 1 failed")
  assertEqual(bitwiseANDRange(12, 17), 0, "Test case 2 failed")
  assertEqual(bitwiseANDRange(19, 24), 16, "Test case 3 failed")
  
  console.log "All tests passed"

main()