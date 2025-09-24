

# Calculates the minimum time required for a character to reach the top of a building with varying floor heights.
# The character can move up one or two floors at a time, but cannot use the two-floor jump consecutively. 
# Moving up one floor takes one unit of time, while jumping up two floors is instantaneous.
#
# Args:
# - floor_heights (Array): An array of integers representing the height of each floor.
#
# Returns:
# - Integer: The minimum time required to reach the top of the building.
#
# Example:
# >>> minimum_time_to_top([3, 5, 1, 8, 4])
# 1

def minimum_time_to_top(heights)
  n = floor_heights.size
  return 0 if n == 0

  # dp[i][0] represents the min time to reach floor i with the last move being a single step (1 floor)
  # dp[i][1] represents the min time to reach floor i with the last move being a jump (2 floors)
  dp = Array.new(n) { [Float::INFINITY, Float::INFINITY] }

  # Base cases
  dp[0][0] = 0  # starting at floor 0 with 0 time (no moves)
  if n >= 1
    dp[1][0] = 1  # reach floor 1 with one step (time 1)
  end

  (2...n).each do |i|
    # Option 1: came from i-1 with a single step (so last move was single step, can choose single or jump next)
    # But since we're at i now, the move before was to i-1, which could be single or jump.
    # So for current i, if the previous move was single (i.e., came from i-1 with single step), then:
    # current move can be single (i-1 -> i) or jump (i-1 -> i+1, but i+1 might be out of bounds)
    # But in DP terms, for dp[i][0], it's the min time to reach i with last move single.
    # So dp[i][0] = min(dp[i-1][0] + 1, dp[i-1][1] + 1) because from i-1, whether last was single or jump, you can take a single step to i.
    dp[i][0] = [dp[i-1][0], dp[i-1][1]].min + 1

    # Option 2: came from i-2 with a jump (so last move was jump, next must be single)
    # So dp[i][1] is only possible if i >= 2, and the move before was a jump (i-2 -> i), so now last move is jump.
    # The time is the time to reach i-2 plus 0 (jump is instantaneous), but the constraint is that the previous move wasn't a jump.
    # Wait, no: the jump is from i-2 to i, so the move before that (i-3 to i-2) could be anything.
    # So dp[i][1] = dp[i-2][0] (because the move before the jump must have been a single step, not another jump)
    if i >= 2
      dp[i][1] = dp[i-2][0]
    end
  end

  # The answer is the min between the two possibilities for the top floor (n-1)
  [dp[n-1][0], dp[n-1][1]].min
end

raise 'Test failed' unless minimum_time_to_top([3, 5, 1, 8, 4]) == 1
raise 'Test failed' unless minimum_time_to_top([2, 2, 2, 2, 2]) == 2
raise 'Test failed' unless minimum_time_to_top([1, 1, 1, 1, 1]) == 1
raise 'Test failed' unless minimum_time_to_top([10]) == 0



puts 'All tests passed!'