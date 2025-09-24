

# Given a grid of non-negative integers grid, find the minimum sum of a path from the top-left corner to the bottom-right corner.
# Each time, you can move either down or right by one step.
# @param grid A 2D array of non-negative integers.
# @return The minimum sum of a path from the top-left corner to the bottom-right corner.
#
# Example 1:
# Input: grid = [[1,3,1],[1,5,1],[4,2,1]]
# Output: 7
# Explanation: The minimum sum path is 1->3->1->1->1.
#
# Example 2:
# Input: grid = [[1,2,3],[4,5,6]]
# Output: 12
#
# @param grid The grid of non-negative integers.
# @return The minimum sum of a path from the top-left corner to the bottom-right corner.
minPathSum = (grid) ->
  return 0 if grid.length == 0 or grid[0].length == 0

  rows = grid.length
  cols = grid[0].length

  # Create a DP table with the same dimensions as the grid
  dp = new Array(rows)
  for i in [0...rows]
    dp[i] = new Array(cols).fill(0)

  # Initialize the starting point
  dp[0][0] = grid[0][0]

  # Fill the first row
  for j in [1...cols]
    dp[0][j] = dp[0][j - 1] + grid[0][j]

  # Fill the first column
  for i in [1...rows]
    dp[i][0] = dp[i - 1][0] + grid[i][0]

  # Fill the rest of the table
  for i in [1...rows]
    for j in [1...cols]
      dp[i][j] = Math.min(dp[i - 1][j], dp[i][j - 1]) + grid[i][j]

  # The result is in the bottom-right corner of the DP table
  dp[rows - 1][cols - 1]
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  # Test case 1
  grid1 = [[1, 4, 7], [2, 5, 1], [8, 3, 6]]
  assertEqual(minPathSum(grid1), 15, 'Test case 1 failed')

  # Test case 2
  grid2 = [[3, 2, 1], [4, 5, 1], [1, 2, 3]]
  assertEqual(minPathSum(grid2), 10, 'Test case 2 failed')

  # Test case 3
  grid3 = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
  assertEqual(minPathSum(grid3), 30, 'Test case 3 failed')

  console.log('All tests passed')

main()