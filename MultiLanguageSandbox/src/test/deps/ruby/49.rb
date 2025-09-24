

# Finds the missing (broken) and duplicate ID in a sequence of ticket IDs.
# The function takes a nested array (array of arrays) where each sub-array
# contains ticket IDs for a particular batch. It returns an array with two elements:
# the first is the missing ID, and the second is the duplicate ID.
#
# Example:
# >>> find_missing_and_duplicate_ids([[5, 6, 8, 11, 9], [10, 12, 9]])
# [7, 9]
# >>> find_missing_and_duplicate_ids([[1, 2, 4], [3, 5, 5]])
# [6, 5]

def find_missing_and_duplicate_ids(ids)
  # Flatten the nested array into a single array
  flattened = ids.flatten
  # Sort the flattened array to easily find duplicates and  sorted = flattened.sort
  # Find the duplicate by  duplicate = nil
  (0...sorted.length - 1).each do |i|
    if sorted[i] == sorted[i + 1]
      duplicate = sorted[i]
      break
    end
  end
  
  # Find the missing number in the sequence from min to max
  min_id = sorted.min
  max_id = sorted.max
  expected_numbers = (min_id..max_id).to_a
  actual_numbers = sorted.uniq
  missing = (expected_numbers - actual_numbers).first
  
  [missing, duplicate]
end

raise 'Test failed' unless find_missing_and_duplicate_ids([[5, 6, 8, 11, 9], [10, 12, 9]]) == [7, 9]
raise 'Test failed' unless find_missing_and_duplicate_ids([[1, 2, 4], [3, 5, 5, 7]]) == [6, 5]
raise 'Test failed' unless find_missing_and_duplicate_ids([[20, 21, 23], [22, 24, 24, 26]]) == [25, 24]
raise 'Test failed' unless find_missing_and_duplicate_ids([[100, 102, 106], [101, 103, 103, 105]]) == [104, 103]