

# Given an integer n, return the nth Fibonacci number.
# The Fibonacci sequence is defined as follows:
# F0 = 0, F1 = 1, F2 = 1, and for n >= 2, Fn = Fn-1 + Fn-2.
# @param n An integer representing the sequence number.
# @return The nth Fibonacci number.
#
# Example 1:
# Input: n = 4
# Output: 3
#
# Example 2:
# Input: n = 25
# Output: 75025
#
# Example 3:
# Input: n = 10
# Output: 55
fibonacci = (n) ->
  if n == 0
    return 0
  if n == 1 or n == 2
    return 1
  
  a = 0
  b = 1
  for i in [2..n]
    c = a + b
    a = b
    b = c
  b
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected

# Test cases
main = ->
  assertEqual fibonacci(4), 3, "Test Case 1 Failed"
  assertEqual fibonacci(25), 75025, "Test Case 2 Failed"
  assertEqual fibonacci(10), 55, "Test Case 3 Failed"
  console.log "All tests passed"

main()