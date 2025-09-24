
class StockTrader {
    static int maxProfit(List<Integer> prices) {
        """
        Given a list of stock prices for each day, find out the maximum profit that can be achieved with at most two transactions.
        Note: You may not engage in multiple transactions simultaneously (i.e., you must sell the stock before you buy again).
        >>> maxProfit([3,3,5,0,0,3,1,4])
        6
        >>> maxProfit([1,2,3,4,5])
        4
        >>> maxProfit([7,6,4,3,1])
        0
        >>> maxProfit([1])
        0
        """

    if (prices == null || prices.size() < 2) {
        return 0
    }
    
    int n = prices.size()
    int[] leftProfits = new int[n]
    int minPrice = prices[0]
    int maxProfit = 0
    
    // First pass: compute max profit for each day if sold on or before that day
    for (int i = 1; i < n; i++) {
        minPrice = Math.min(minPrice, prices[i])
        maxProfit = Math.max(maxProfit, prices[i] - minPrice)
        leftProfits[i] = maxProfit
    }
    
    int[] rightProfits = new int[n]
    int maxPrice = prices[n - 1]
    maxProfit = 0
    
    // Second pass: compute max profit for each day if bought on or after that day
    for (int i = n - 2; i >= 0; i--) {
        maxPrice = Math.max(maxPrice, prices[i])
        maxProfit = Math.max(maxProfit, maxPrice - prices[i])
        rightProfits[i] = maxProfit
    }
    
    // Find the maximum sum of leftProfits[i] and rightProfits[i+1]
    int totalMax = 0
    for (int i = 0; i < n - 1; i++) {
        totalMax = Math.max(totalMax, leftProfits[i] + rightProfits[i + 1])
    }
    
    // Also consider the case where only one transaction is done (max of leftProfits)
    totalMax = Math.max(totalMax, leftProfits[n - 1])
    
    return totalMax
}
}
// Test cases
def stockTrader = new StockTrader()
assert stockTrader.maxProfit([3,3,5,0,0,3,1,4]) == 6
assert stockTrader.maxProfit([1,2,3,4,5]) == 4
assert stockTrader.maxProfit([7,6,4,3,1]) == 0
assert stockTrader.maxProfit([1]) == 0
assert stockTrader.maxProfit([2,1,2,0,1]) == 2
assert stockTrader.maxProfit([3,2,6,5,0,3]) == 7
assert stockTrader.maxProfit([1,2,4,2,5,7,2,4,9,0]) == 13