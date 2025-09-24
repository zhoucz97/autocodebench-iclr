

# Given a number N, find the smallest prime palindrome greater than or equal to N.
# @param N An integer representing the input number.
# @return The smallest prime palindrome greater than or equal to N.
#
# Example 1:
# Input: N = 6
# Output: 7
#
# Example 2:
# Input: N = 8
# Output: 11
#
# Example 3:
# Input: N = 13
# Output: 101
#
# Constraints:
# 1 <= N <= 10^8
# The answer is guaranteed to exist and be less than 2 * 10^8.
smallestPrimePalindrome = (N) ->
  isPrime = (num) ->
    return false if num < 2
    return true if num == 2
    return false if num % 2 == 0
    sqrtNum = Math.sqrt(num)
    i = 3
    while i <= sqrtNum
      return false if num % i == 0
      i += 2
    true

  isPalindrome = (num) ->
    str = num.toString()
    str == str.split('').reverse().join('')

  current = N
  while true
    if isPalindrome(current) and isPrime(current)
      return current
    current += 1
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test Cases
main = ->
  assertEqual(smallestPrimePalindrome(7), 7)
  assertEqual(smallestPrimePalindrome(9), 11)
  assertEqual(smallestPrimePalindrome(15), 101)

  console.log("All tests passed")

main()