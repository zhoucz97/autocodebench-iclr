

assert = require 'assert'
hasCloseElements = (numbers, threshold) ->
  """
  Check if in given list of numbers, any two numbers are closer to each other than
  given threshold.
  >>> has_close_elements([1.0, 2.0, 3.0], 0.5)
  False
  >>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)
  True
  """
  # Sort the array to easily find the closest elements
  sortedNumbers = numbers.slice().sort((a, b) -> a - b)
  
  # Iterate through the sorted array and check adjacent elements
  for i in [0...sortedNumbers.length - 1]
    if Math.abs(sortedNumbers[i] - sortedNumbers[i + 1]) < threshold
      return true
  
  false
# Test cases
check = (hasCloseElements) ->
  assert hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3) is true
  assert hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05) is false
  assert hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95) is true
  assert hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8) is false
  assert hasCloseElements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1) is true
  assert hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0) is true
  assert hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5) is false

check hasCloseElements