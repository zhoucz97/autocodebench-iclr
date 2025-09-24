

###
 Calculate the number of times the digit 1 appears in all non-negative integers less than or equal to n.

 @param n An integer representing the upper limit.
 @return The number of times the digit 1 appears.

 Example:
 countDigitOne(13)
 Output: 6

 Example:
 countDigitOne(0)
 Output: 0
###
countDigitOne = (n) ->
  return 0 if n < 1
  count = 0
  i = 1  # current digit position (1 for units, 10 for tens, etc.)
  while i <= n
    # Split the number into higher, current, and lower parts
    higher = Math.floor(n / (i * 10))
    current = Math.floor((n / i) % 10)
    lower = n % i
    
    if current == 0
      count += higher * i
    else if current == 1
      count += higher * i + lower + 1
    else
      count += (higher + 1) * i
    
    i *= 10
  count
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless JSON.stringify(actual) is JSON.stringify(expected) 

# Test cases
assertEqual countDigitOne(20), 12, "Test case 1 failed"
assertEqual countDigitOne(55), 16, "Test case 2 failed"
assertEqual countDigitOne(99), 20, "Test case 3 failed"
console.log 'All tests passed'