

# Finds the length of the longest consecutive sequence in an array of integers.
# The function takes an array of integers as an argument and returns the length
# of the longest consecutive sequence of numbers present in the array.
#
# Example:
# >>> longest_consecutive_sequence([1, 5, 6, 2, 3, 4, 5, 6, 8, 9])
# 6  # Because the longest consecutive sequence is 1, 2, 3, 4, 5, 6
# >>> longest_consecutive_sequence([10, 4, 20, 1, 3, 2])
# 4  # Because the longest consecutive sequence is 1, 2, 3, 4
def longest_consecutive_sequence(arr)
  return 0 if arr.empty?

  num_set = arr.to_set
  max_length = 0

  num_set.each do |num|
    # Only    unless num_set.include?(num - 1)
      current_num = num
      current_length = 1

      while num_set.include?(current_num + 1)
        current_num += 1
        current_length += 1
      end

      max_length = [max_length, current_length].max
    end
  end

  max_length
end

raise 'Test failed' unless longest_consecutive_sequence([1, 5, 6, 2, 3, 4, 5, 6, 8, 9]) ==6
raise 'Test failed' unless longest_consecutive_sequence([10, 4, 20, 1, 3, 2]) == 4
raise 'Test failed' unless longest_consecutive_sequence([100, 4, 200, 1, 3, 2]) == 4
raise 'Test failed' unless longest_consecutive_sequence([1, 2, 0, 1]) == 3
raise 'Test failed' unless longest_consecutive_sequence([]) == 0
raise 'Test failed' unless longest_consecutive_sequence([1]) == 1