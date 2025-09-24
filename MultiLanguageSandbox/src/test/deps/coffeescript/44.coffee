

# Given a non-decreasing sorted integer array, find the integer that appears more than 25% of the total number of elements in the array.
# @param arr A non-decreasing sorted integer array.
# @return The integer that appears more than 25% of the total number of elements in the array.
#
# Example:
# Input: arr = [1,2,2,6,6,6,6,7,10]
# Output: 6
#
findSpecialInteger = (arr) ->
  n = arr.length
  quarter = Math.floor(n / 4)
  
  # Check the candidates at positions i, i + quarter, i + 2*quarter, etc.
  for i in [0...quarter + 1]
    if arr[i] == arr[i + quarter]
      return arr[i]
  
  # If no candidate found in the first quarter, check the rest
  for i in [quarter + 1...n - quarter]
    if arr[i] == arr[i + quarter]
      return arr[i]
  
  # As per problem statement, there must be a solution
  return arr[n - 1]
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual == expected
main = ->
  # Test Case 1
  testArr1 = [1,1,2,2,3,3,3,3]
  assertEqual(findSpecialInteger(testArr1), 3)

  # Test Case 2
  testArr2 = [1]
  assertEqual(findSpecialInteger(testArr2), 1)

  # Test Case 3
  testArr3 = [10,10,10,20,30,30,30,30,40,50,50,60,70,80,90]
  assertEqual(findSpecialInteger(testArr3), 30)

  console.log "All tests passed"

main()