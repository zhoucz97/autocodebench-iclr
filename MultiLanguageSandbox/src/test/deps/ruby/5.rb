

# Finds the maximum possible Least Common Multiple (LCM) of any three numbers chosen between 1 and n.
# The function takes a single argument, n, which is a positive integer, and returns the highest LCM
# attainable by any combination of three distinct numbers from 1 to n.
#
# Example:
# >>> max_lcm_of_three(9)
# 504
# >>> max_lcm_of_three(10)
# 630

def max_lcm_of_three(n)
  return 0 if n < 3

  max_lcm = 0

  # We need to  # The optimal candidates are usually near the top, so we  candidates = []
  # Add the top three numbers
  candidates << [n, n-1, n-2] if n >= 3
  # Add cases where one of the numbers is n-3, etc., to cover possible higher LCMs
  candidates << [n, n-1, n-3] if n >= 4
  candidates << [n, n-2, n-3] if n >= 5
  candidates << [n-1, n-2, n-3] if n >= 6
  # Also  candidates << [n, n-2, n-4] if n >= 5
  candidates << [n-1, n-3, n-4] if n >= 6

  # Remove duplicates and ensure all numbers are within 1..n
  unique_candidates = candidates.uniq.select { |a, b, c| a.between?(1, n) && b.between?(1, n) && c.between?(1, n) }

  unique_candidates.each do |a, b, c|
    current_lcm = lcm(a, b, c)
    max_lcm = current_lcm if current_lcm > max_lcm
  end

  max_lcm
end

def gcd(a, b)
  b == 0 ? a : gcd(b, a % b)
end

def lcm(a, b)
  (a * b) / gcd(a, b)
end

def lcm(a, b, c)
  lcm_ab = lcm(a, b)
  lcm(lcm_ab, c)
end

raise 'Test failed' unless max_lcm_of_three(9) == 504
raise 'Test failed' unless max_lcm_of_three(10) == 630
raise 'Test failed' unless max_lcm_of_three(3) == 6
raise 'Test failed' unless max_lcm_of_three(5) == 60
raise 'Test failed' unless max_lcm_of_three(1) == 1
  

puts 'All tests passed!'