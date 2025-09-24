
class CoinChange {
    static int minCoins(ArrayList coins, int amount) {
        """
        Given an array of coin denominations and a total amount of money,
        return the fewest number of coins that you need to make up that amount.
        If that amount of money cannot be made up by any combination of the coins, return -1.
        You may assume that you have an infinite number of each kind of coin.
        >>> minCoins([1, 2, 5], 11)
        3
        >>> minCoins([2], 3)
        -1
        >>> minCoins([1], 0)
        0
        """

    // Create a DP array where dp[i] represents the minimum number of coins needed for amount i
    int[] dp = new int[amount + 1]
    
    // Initialize all values to a large number (infinity)
    Arrays.fill(dp, Integer.MAX_VALUE)
    
    // Base case: 0 coins needed for amount 0
    dp[0] = 0
    
    // Fill the DP array
    for (int i = 1; i <= amount; i++) {
        for (int coin in coins) {
            if (coin <= i && dp[i - coin] != Integer.MAX_VALUE) {
                dp[i] = Math.min(dp[i], dp[i - coin] + 1)
            }
        }
    }
    
    // Return the result or -1 if it's still Integer.MAX_VALUE
    return dp[amount] == Integer.MAX_VALUE ? -1 : dp[amount]
}
}
// Test cases
def coinChange = new CoinChange()
assert coinChange.minCoins([1, 2, 5], 11) == 3
assert coinChange.minCoins([2], 3) == -1
assert coinChange.minCoins([1], 0) == 0
assert coinChange.minCoins([1, 2, 5], 10) == 2
assert coinChange.minCoins([2, 5, 10, 1], 27) == 4
assert coinChange.minCoins([5, 10, 25], 63) == -1
assert coinChange.minCoins([1, 5, 10, 25], 100) == 4