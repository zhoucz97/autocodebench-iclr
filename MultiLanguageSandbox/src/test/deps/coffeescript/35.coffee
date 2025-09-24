

# Given an integer, convert it to a hexadecimal string.
# For negative integers, use two's complement method.
# @param num The integer to be converted.
# @return The hexadecimal string representation of the integer.
#
# Example 1:
# Input: num = 26
# Output: "1a"
#
# Example 2:
# Input: num = -1
# Output: "ffffffff"
toHex = (num) ->
  if num == 0
    return "0"
  
  # Handle negative numbers using two's complement
  if num < 0
    num = (1 << 32) + num
  
  hexChars = "0123456789abcdef"
  result = ""
  
  while num > 0
    digit = num % 16
    result = hexChars[digit] + result
    num = Math.floor(num / 16)
  
  return result
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  assertEqual toHex(305419896), '12345678', 'Test Case 1 Failed'
  assertEqual toHex(-42), 'ffffffd6', 'Test Case 2 Failed'
  assertEqual toHex(1234), '4d2', 'Test Case 3 Failed'
  console.log "All tests passed"

main()