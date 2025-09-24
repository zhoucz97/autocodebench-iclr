

# Counts the number of numbers less than or equal to n that are either multiples of 7 or contain the digit '7'.
# The function takes a single argument, n, which is a positive integer, and returns the count of numbers 
# that are related to 7 as per the criteria.
#
# Example:
# >>> count_seven_related_numbers(20)
# 3
# >>> count_seven_related_numbers(15)
# 2

def count_seven_related_numbers(n)
  count = 0
  (1..n).each do |num|
    if num % 7 == 0 || num.to_s.include?('7')
      count += 1
    end
  end
  count
end

raise 'Test failed' unless count_seven_related_numbers(20) == 3
raise 'Test failed' unless count_seven_related_numbers(15) == 2
raise 'Test failed' unless count_seven_related_numbers(50) == 11
raise 'Test failed' unless count_seven_related_numbers(28) == 6
raise 'Test failed' unless count_seven_related_numbers(100) == 30
  

puts 'All tests passed!'