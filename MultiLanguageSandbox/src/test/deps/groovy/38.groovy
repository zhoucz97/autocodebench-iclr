
class StockManager {
    static List<Integer> getLeastStockItems(List<Integer> stock, int cnt) {
        """
        Given a list of stock quantities and a count, return the least stock quantities.
        If there are multiple items with the same least stock, return them all.
        >>> getLeastStockItems([2, 5, 7, 4], 1)
        [2]
        >>> getLeastStockItems([0, 2, 3, 6], 2)
        [0, 2] or [2, 0]
        """

    // Sort the stock list in ascending order
    def sortedStock = stock.sort()
    
    // Take the first 'cnt' elements from the sorted list
    def leastStockItems = sortedStock.take(cnt)
    
    return leastStockItems
}
}
// Test cases
def stockManager = new StockManager()
assert stockManager.getLeastStockItems([2, 5, 7, 4], 1) == [2]
assert stockManager.getLeastStockItems([0, 2, 3, 6], 2) == [0, 2] || stockManager.getLeastStockItems([0, 2, 3, 6], 2) == [2, 0]
assert stockManager.getLeastStockItems([10, 20, 30, 40], 3) == [10, 20, 30]
assert stockManager.getLeastStockItems([50, 40, 30, 20, 10], 4) == [10, 20, 30, 40]
assert stockManager.getLeastStockItems([1, 1, 1, 1], 2) == [1, 1]
assert stockManager.getLeastStockItems([100, 200, 300, 400], 1) == [100]
assert stockManager.getLeastStockItems([5, 5, 5, 5], 3) == [5, 5, 5]