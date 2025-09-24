

# Given an integer n, return the largest number less than or equal to n that is monotonically increasing.
# @param n An integer.
# @return The largest monotonically increasing number less than or equal to n.
#
# Example 1:
# Input: n = 10
# Output: 9
#
# Example 2:
# Input: n = 1234
# Output: 1234
#
# Example 3:
# Input: n = 332
# Output: 299
maxMonotonicNumber = (n) ->
  s = n.toString()
  len = s.length
  i = 1
  while i < len
    if s[i] < s[i-1]
      # Need to decrement the previous digit and set current and following to 9
      j = i - 1
      while j >= 0 and s[j] == '0'
        j--
      if j < 0
        # All digits before are zero, so the number becomes 9...9 (len-1 digits)
        return Math.pow(10, len - 1) - 1
      s = s.substring(0, j) + (parseInt(s[j]) - 1).toString() + '9'.repeat(len - j - 1)
      # Recursively check the new number
      return maxMonotonicNumber(parseInt(s))
    i++
  parseInt(s)
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test cases
main = ->
  assertEqual(maxMonotonicNumber(15), 15, 'Test Case 1 Failed')
  assertEqual(maxMonotonicNumber(87654321), 79999999, 'Test Case 2 Failed')
  assertEqual(maxMonotonicNumber(987654), 899999, 'Test Case 3 Failed')

  console.log('All tests passed')

main()