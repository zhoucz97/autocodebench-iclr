

# Calculates the sum of two integers as perceived by Xiao Ming, a child who 
# simplifies numbers larger than 99 to their last two digits before addition, 
# and also keeps only the last two digits of the result if it exceeds 99.
#
# The function takes two arguments, a and b, which are non-negative integers, and 
# returns the final sum according to Xiao Ming's calculation method.
#
# Examples:
# >>> ming_sum(35, 80)
# 15
# >>> ming_sum(15, 1152)
# 67

def ming_sum(a, b)
  # Simplify numbers larger than 99 to their last two digits
  a = a % 100 if a > 99
  b = b % 100 if b > 99
  
  # Calculate the sum
  sum = a + b
  
  # Keep only the last two digits if the sum exceeds 99
  sum = sum % 100 if sum > 99
  
  sum
end

raise 'Test failed' unless ming_sum(35, 80) == 15
raise 'Test failed' unless ming_sum(15, 1152) == 67
raise 'Test failed' unless ming_sum(1234, 5678) == 12
raise 'Test failed' unless ming_sum(0, 99) == 99
raise 'Test failed' unless ming_sum(50, 50) == 0




puts 'All tests passed!'