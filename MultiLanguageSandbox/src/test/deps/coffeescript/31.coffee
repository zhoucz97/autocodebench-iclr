

# Given the number of passengers n, calculate the probability that the last passenger will sit in their own seat.
# @param n The number of passengers.
# @return The probability that the last passenger will sit in their own seat.
#
# Example 1:
# Input: n = 6
# Output: 0.5
#
# Example 2:
# Input: n = 10
# Output: 0.5
#

calculateProbability = (n) ->
  if n < 2 then 0 else 0.5
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless JSON.stringify(actual) is JSON.stringify(expected) 
# Test cases
main = ->
    assertEqual(calculateProbability(6), 0.5, "Expected the probability for 6 passengers to be 0.5")
    assertEqual(calculateProbability(10), 0.5, "Expected the probability for 10 passengers to be 0.5")
    assertEqual(calculateProbability(20), 0.5, "Expected the probability for 20 passengers to be 0.5")
    
    console.log("All tests passed")
    
main()