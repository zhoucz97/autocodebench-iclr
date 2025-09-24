
class StairClimber {
    static int climbStairs(int n) {
        """
        Given an integer n, representing the number of stairs to reach the top.
        Each time you can either climb 1 or 2 stairs.
        Return the number of distinct ways you can climb to the top.
        >>> climbStairs(2)
        2
        >>> climbStairs(3)
        3
        """

        if (n <= 1) return 1
        
        int[] dp = new int[n + 1]
        dp[0] = 1
        dp[1] = 1
        
        for (int i = 2; i <= n; i++) {
            dp[i] = dp[i - 1] + dp[i - 2]
        }
        
        return dp[n]
    }
}
// Test cases
def stairClimber = new StairClimber()
assert stairClimber.climbStairs(1) == 1
assert stairClimber.climbStairs(2) == 2
assert stairClimber.climbStairs(3) == 3
assert stairClimber.climbStairs(4) == 5
assert stairClimber.climbStairs(5) == 8
assert stairClimber.climbStairs(6) == 13
assert stairClimber.climbStairs(7) == 21
assert stairClimber.climbStairs(8) == 34