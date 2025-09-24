

# Given a set of letter tiles, each represented by a letter in `tiles`, return the number of possible non-empty letter sequences that can be printed.
# @param tiles A string representing the set of letter tiles.
# @return The number of possible letter sequences.
#
# Example 1:
# Input: tiles = "AAB"
# Output: 8
#
# Example 2:
# Input: tiles = "AAABBC"
# Output: 188
#
# Example 3:
# Input: tiles = "V"
# Output: 1
numTilePossibilities = (tiles) ->
  count = {}
  for char in tiles
    count[char] ?= 0
    count[char] += 1
  result = 0

  backtrack = (current) ->
    if current.length > 0
      result += 1
    for char, cnt of count
      if cnt > 0
        count[char] -= 1
        backtrack(current + char)
        count[char] += 1

  backtrack("")
  result
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Testing
main = ->
  # Test case 1
  tiles = "ABC"
  expected = 15
  assertEqual(numTilePossibilities(tiles), expected, "Test case 1 failed")

  # Test case 2
  tiles = "ABCD"
  expected = 64
  assertEqual(numTilePossibilities(tiles), expected, "Test case 2 failed")

  # Test case 3
  tiles = "XYZ"
  expected = 15
  assertEqual(numTilePossibilities(tiles), expected, "Test case 3 failed")

  console.log("All tests passed")

main()