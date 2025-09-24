

# Finds the maximum sum of any three numbers from a given array that is divisible by a specified number k.
# This function takes an array of numbers and an integer k as arguments. It returns the maximum sum 
# of any three numbers from the array such that this sum is divisible by k. It's guaranteed that there 
# are at least three numbers in the array and that a solution exists.
#
# Example:
# >>> max_sum_divisible_by_k([1, 2, 3, 4], 3)
# 9
# >>> max_sum_divisible_by_k([4, 5, 10, 20], 5)
# 35

def max_sum_divisible_by_k(numbers, k)
  max_sum = -Float::INFINITY
  # Generate all possible combinations of three numbers
  numbers.combination(3).each do |a, b, c|
    current_sum = a + b + c
    if current_sum % k == 0 && current_sum > max_sum
      max_sum = current_sum
    end
  end
  max_sum
end

raise 'Test failed' unless max_sum_divisible_by_k([1, 2, 3, 4], 3) == 9
raise 'Test failed' unless max_sum_divisible_by_k([4, 5, 10, 20], 5) == 35
raise 'Test failed' unless max_sum_divisible_by_k([6, 12, 17, 23, 29], 7) == 35
raise 'Test failed' unless max_sum_divisible_by_k([7, 14, 21, 28], 7) == 63
raise 'Test failed' unless max_sum_divisible_by_k([10, 15, 25, 30, 40], 10) == 80


puts 'All tests passed!'