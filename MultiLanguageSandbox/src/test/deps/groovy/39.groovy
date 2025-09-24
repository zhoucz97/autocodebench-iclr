
class JumpWaysCalculator {
    static int calculateJumpWays(int num) {
        """
        Calculate the number of different ways a student can jump across a platform with 'num' cells.
        The student can jump one cell or two cells at a time.
        The result is returned modulo 1e9+7 to prevent it from being too large.
        >>> calculateJumpWays(2)
        2
        >>> calculateJumpWays(5)
        8
        """

    if (num == 0) return 1
    if (num == 1) return 1
    
    int mod = 1_000_000_007
    int[] dp = new int[num + 1]
    dp[0] = 1
    dp[1] = 1
    
    for (int i = 2; i <= num; i++) {
        dp[i] = (dp[i - 1] + dp[i - 2]) % mod
    }
    
    return dp[num]
}
}
// Test cases
def jumpWaysCalculator = new JumpWaysCalculator()
assert jumpWaysCalculator.calculateJumpWays(2) == 2
assert jumpWaysCalculator.calculateJumpWays(5) == 8
assert jumpWaysCalculator.calculateJumpWays(10) == 89
assert jumpWaysCalculator.calculateJumpWays(20) == 10946
assert jumpWaysCalculator.calculateJumpWays(50) == 365010934
assert jumpWaysCalculator.calculateJumpWays(100) == 782204094