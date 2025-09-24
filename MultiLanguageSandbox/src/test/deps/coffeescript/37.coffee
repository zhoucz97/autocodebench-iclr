

# Given an integer num, determine whether it is a perfect number.
# A perfect number is a positive integer that is equal to the sum of its proper positive divisors.
# @param num An integer.
# @return True if num is a perfect number, False otherwise.
#
# Example 1:
# Input: num = 28
# Output: true
# Explanation: 28 = 1 + 2 + 4 + 7 + 14
# 1, 2, 4, 7, and 14 are all the proper positive divisors of 28.
#
# Example 2:
# Input: num = 7
# Output: false
#
# Constraints:
# 1 <= num <= 10^8
isPerfectNumber = (num) ->
  return false if num <= 1  # Perfect numbers are greater than 1
  
  sum = 1  # 1 is a proper divisor for all numbers > 1
  sqrtNum = Math.sqrt(num)
  
  for i in [2..sqrtNum]
    if num % i == 0
      sum += i
      otherDivisor = num / i
      sum += otherDivisor unless otherDivisor == i
  
  sum == num
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  # Test cases
  assertEqual isPerfectNumber(6), true, "6 should be a perfect number"
  assertEqual isPerfectNumber(496), true, "496 should be a perfect number"
  assertEqual isPerfectNumber(8128), true, "8128 should be a perfect number"
  console.log "All tests passed"
  
main()