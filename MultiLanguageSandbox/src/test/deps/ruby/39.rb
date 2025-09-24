

# Calculates the total number of ways to safely place nuclear materials in N pits arranged in a line
# such that no three consecutive pits contain nuclear materials, avoiding an explosion.
# The function takes a single argument, n, which is a positive integer representing the number of pits,
# and returns the total count of safe placement combinations for the given number of pits.
#
# Example:
# >>> count_safe_placements(1)
# 2
# >>> count_safe_placements(4)
# 13
# >>> count_safe_placements(10)
# 504

def count_safe_placements(n)
  return 2 if n == 1
  return 4 if n == 2

  # dp[i][j] where j represents the state:
  # 0: last pit is empty
  # 1: last pit has material, previous is empty
  # 2: last two pits have material, previous to those is empty
  dp = Array.new(n + 1) { Array.new(3, 0) }

  # Base cases
  dp[1][0] = 1  # one way: empty
  dp[1][1] = 1  # one way: with material
  dp[1][2] = 0  # impossible for n=1

  dp[2][0] = 2  # both empty or first with material, second empty
  dp[2][1] = 1  # first empty, second with material
  dp[2][2] = 1  # both with material

  (3..n).each do |i|
    # Current pit is empty (state 0): previous can be any state
    dp[i][0] = dp[i-1][0] + dp[i-1][1] + dp[i-1][2]

    # Current pit has material, previous was empty (state 1)
    dp[i][1] = dp[i-1][0]

    # Current pit has material, previous had material, the one before was empty (state 2)
    dp[i][2] = dp[i-1][1]
  end

  dp[n][0] + dp[n][1] + dp[n][2]
end

raise 'Test failed' unless count_safe_placements(1) == 2
raise 'Test failed' unless count_safe_placements(4) == 13
raise 'Test failed' unless count_safe_placements(10) == 504
raise 'Test failed' unless count_safe_placements(3) == 7
raise 'Test failed' unless count_safe_placements(2) == 4
    

puts 'All tests passed!'