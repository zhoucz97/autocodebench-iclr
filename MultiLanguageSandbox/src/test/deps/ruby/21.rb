

# This method calculates the minimum number of steps required to transform a given number into a palindrome by repeatedly reversing its digits and adding the result to the original number. The process is stopped when a palindrome is obtained. If the palindrome is not obtained within 8 steps, the method returns 0.
# The function takes a single argument, m, which is a positive integer between 12 and 100, and returns the number of steps taken to reach a palindrome. The input number m itself is not a palindrome.
#
# Examples:
# >>> palindrome_sum_steps(12)
# 1
# >>> palindrome_sum_steps(87)
# 4
# >>> palindrome_sum_steps(89)
# 0

def palindrome_sum_steps(m)
  steps = 0
  current = m
  
  while steps < 8
    reversed = current.to_s.reverse.to_i
    if current == reversed
      return steps
    end
    current += reversed
    steps += 1
  end
  
  0
end

raise 'Test failed' unless palindrome_sum_steps(12) == 1
raise 'Test failed' unless palindrome_sum_steps(87) == 4
raise 'Test failed' unless palindrome_sum_steps(89) == 0
raise 'Test failed' unless palindrome_sum_steps(56) == 1
raise 'Test failed' unless palindrome_sum_steps(95) == 3

puts 'All tests passed!'