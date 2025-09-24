

# Counts the number of times the digit '1' appears in all numbers from 1 to n.
# The function takes a single argument, n, which is a positive integer, and 
# returns the total count of the digit '1' appearing in all numbers from 1 to n.
#
# Example:
# >>> count_ones_in_range(15)
# 8
# >>> count_ones_in_range(20)
# 12

def count_ones_in_range(n)
  count = 0
  i = 1 # current digit position (1 for units, 10 for tens, etc.)
  
  while i <= n
    # Split the number into higher, current, and lower parts
    higher = n / (i * 10)
    current = (n / i) % 10
    lower = n % i
    
    if current == 0
      count += higher * i
    elsif current == 1
      count += higher * i + lower + 1
    else
      count += (higher + 1) * i
    end
    
    i *= 10
  end
  
  count
end

raise 'Test failed' unless count_ones_in_range(15) == 8
raise 'Test failed' unless count_ones_in_range(20) == 12
raise 'Test failed' unless count_ones_in_range(100) == 21
raise 'Test failed' unless count_ones_in_range(5) == 1
raise 'Test failed' unless count_ones_in_range(55) == 16
raise 'Test failed' unless count_ones_in_range(0) == 0

puts 'All tests passed!'