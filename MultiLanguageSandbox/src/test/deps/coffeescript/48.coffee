

# Given an integer n, determine if it is a power of two.
# If it is, return true; otherwise, return false.
# A number n is a power of two if there exists an integer x such that n == 2^x.

# @param n The integer to be checked.
# @return True if n is a power of two, false otherwise.
#
# Example:
# isPowerOfTwo(1)
# Output: true
#
# Example:
# isPowerOfTwo(16)
# Output: true
#
# Example:
# isPowerOfTwo(3)
# Output: false
#
# Example:
# isPowerOfTwo(4)
# Output: true
#
# Example:
# isPowerOfTwo(5)
# Output: false

isPowerOfTwo = (n) ->
  return false if n <= 0
  (n & (n - 1)) == 0
# AssertionError class
class AssertionError extends Error

# Assert function
assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless JSON.stringify(actual) is JSON.stringify(expected) 
# Test cases using assert function
main = ->
  assertEqual isPowerOfTwo(1024), true, "1024 is 2^10"
  assertEqual isPowerOfTwo(65536), true, "65536 is 2^16"
  assertEqual isPowerOfTwo(14), false, "14 is not a power of two"

  console.log "All tests passed"

main()