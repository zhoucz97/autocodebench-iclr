

# Calculates the total count of the digit '1' in the binary representation of all numbers in a given range [l, r].
# The function takes two arguments, l and r, where l <= r and both are non-negative integers. It returns the sum 
# of all '1's in the binary representation of each number in the range [l, r].
#
# Examples:
# >>> sum_of_binary_ones(2, 3)
# 3
# >>> sum_of_binary_ones(5, 5)
# 2
# >>> sum_of_binary_ones(0, 4)
# 5


def sum_of_binary_ones(l, r)
  (l..r).sum { |num| num.to_s(2).count('1') }
end

raise 'Test failed' unless sum_of_binary_ones(2, 3) == 3
raise 'Test failed' unless sum_of_binary_ones(5, 5) == 2
raise 'Test failed' unless sum_of_binary_ones(0, 4) == 5
raise 'Test failed' unless sum_of_binary_ones(10, 15) == 17
raise 'Test failed' unless sum_of_binary_ones(0, 0) == 0



puts 'All tests passed!'