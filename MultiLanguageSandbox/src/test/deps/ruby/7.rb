

# Finds the maximum value in an array and returns its value along with its index.
# The function takes a single argument, arr, which is an array of integers. The array size should be between 1 and 100.
# It returns an array containing the maximum value and its index in the format [max_value, index].
#
# Example:
# >>> find_max_with_index([3, 2, 1])
# [3, 0]
# >>> find_max_with_index([10, 20, 30, 40])
# [40, 3]

def find_max_with_index(arr)
  max_value = arr.max
  max_index = arr.index(max_value)
  [max_value, max_index]
end

raise 'Test failed' unless find_max_with_index([3, 2, 1]) == [3, 0]
raise 'Test failed' unless find_max_with_index([10, 20, 30, 40]) == [40, 3]
raise 'Test failed' unless find_max_with_index([5]) == [5, 0]
raise 'Test failed' unless find_max_with_index([-3, -2, -1, -4]) == [-1, 2]
raise 'Test failed' unless find_max_with_index([100, 200, 100, 50]) == [200, 1]
  
puts 'All tests passed!'