

# Calculates the minimum time required to reverse the order of M people standing in a circle.
# In each minute, only a pair of adjacent people can swap places. The function returns the 
# minimum number of minutes required to reverse the order of the people, such that each person's 
# left neighbor becomes their right neighbor and vice versa.
#
# Args:
# - m: A positive integer representing the number of people in the circle.
#
# Returns:
# - An integer representing the minimum number of minutes required to achieve the reversed order.
#
# Examples:
# >>> min_time_to_reverse_order(4)
# 2
# >>> min_time_to_reverse_order(5)
# 4
# >>> min_time_to_reverse_order(6)
# 6

def min_time_to_reverse_order(m)
  (m / 2) * 2
end

raise 'Test failed' unless min_time_to_reverse_order(4) == 2
raise 'Test failed' unless min_time_to_reverse_order(5) == 4
raise 'Test failed' unless min_time_to_reverse_order(6) == 6
raise 'Test failed' unless min_time_to_reverse_order(7) == 9
raise 'Test failed' unless min_time_to_reverse_order(10) == 20



puts 'All tests passed!'