
class MaximumSalesSum {
    static int maxSalesSum(ArrayList sales) {
        """
        Find the maximum sum of a continuous subarray in the given sales array.
        >>> maxSalesSum([-2,1,-3,4,-1,2,1,-5,4])
        6
        >>> maxSalesSum([5,4,-1,7,8])
        23
        """

    if (sales.isEmpty()) return 0
    
    int maxCurrent = sales[0]
    int maxGlobal = sales[0]
    
    for (int i = 1; i < sales.size(); i++) {
        maxCurrent = Math.max(sales[i], maxCurrent + sales[i])
        if (maxCurrent > maxGlobal) {
            maxGlobal = maxCurrent
        }
    }
    
    return maxGlobal
}
}
// Test cases
def maxSalesSum = new MaximumSalesSum()
assert maxSalesSum.maxSalesSum([1, -3, 2, 1, -1]) == 3
assert maxSalesSum.maxSalesSum([-2, -1, -3, -4, -1]) == -1
assert maxSalesSum.maxSalesSum([5, -4, 1, -7, 8]) == 8
assert maxSalesSum.maxSalesSum([-5, -4, -1, -7, -8]) == -1
assert maxSalesSum.maxSalesSum([1, 2, 3, 4, 5]) == 15
assert maxSalesSum.maxSalesSum([-1, -2, -3, -4, -5]) == -1