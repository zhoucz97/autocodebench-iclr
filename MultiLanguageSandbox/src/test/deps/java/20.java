
class Solution {
    /**
     * Given an integer array cost where cost[i] is the cost of ith step on a staircase,
     * return the minimum cost to reach the top of the floor.
     * You can either start from the step with index 0 or the step with index 1.
     *
     * Example:
     * >>> minCostClimbingStairs([10, 15, 20])
     * 15
     * Explanation: You will start at index 1.
     * - Pay 15 and climb two steps to reach the top.
     * The total cost is 15.
     *
     * >>> minCostClimbingStairs([1, 100, 1, 1, 1, 100, 1, 1, 100, 1])
     * 6
     * Explanation: You will start at index 0.
     * - Pay 1 and climb two steps to reach index 2.
     * - Pay 1 and climb two steps to reach index 4.
     * - Pay 1 and climb two steps to reach index 6.
     * - Pay 1 and climb one step to reach index 7.
     * - Pay 1 and climb two steps to reach index 9.
     * - Pay 1 and climb one step to reach the top.
     * The total cost is 6.
     */
  public int minCostClimbingStairs(int[] cost) 
{
    int n = cost.length;
    if (n == 0) return 0;
    if (n == 1) return cost[0];
    
    int[] dp = new int[n];
    dp[0] = cost[0];
    dp[1] = cost[1];
    
    for (int i = 2; i < n; i++) {
        dp[i] = cost[i] + Math.min(dp[i - 1], dp[i - 2]);
    }
    
    return Math.min(dp[n - 1], dp[n - 2]);
}
  public static void main(String[] args) {
    Solution solution = new Solution();

    // Test the function with different test cases
    assert solution.minCostClimbingStairs(new int[] { 10, 15, 20 }) == 15;
    assert solution.minCostClimbingStairs(
      new int[] { 1, 100, 1, 1, 1, 100, 1, 1, 100, 1 }
    ) ==
    6;
    assert solution.minCostClimbingStairs(new int[] { 0, 0, 0, 1 }) == 0;
    System.out.println("All tests passed");
  }
}
