
class StockTradeChecker {
    static int countInversionPairs(List<Integer> record) {
        """
        Count the total number of inversion pairs in a given record of stock prices.
        An inversion pair is defined as a pair of prices where the earlier price is higher than the later one.
        >>> countInversionPairs([9, 7, 5, 4, 6])
        8
        >>> countInversionPairs([1, 2, 3, 4, 5])
        0
        """

    int count = 0
    int n = record.size()
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            if (record[i] > record[j]) {
                count++
            }
        }
    }
    return count
}
}
// Test cases
def stockTradeChecker = new StockTradeChecker()
assert stockTradeChecker.countInversionPairs([9, 7, 5, 4, 6]) == 8
assert stockTradeChecker.countInversionPairs([1, 2, 3, 4, 5]) == 0
assert stockTradeChecker.countInversionPairs([5, 4, 3, 2, 1]) == 10
assert stockTradeChecker.countInversionPairs([1, 3, 5, 7, 9]) == 0
assert stockTradeChecker.countInversionPairs([10, 8, 6, 4, 2]) == 10
assert stockTradeChecker.countInversionPairs([1, 1, 1, 1, 1]) == 0
assert stockTradeChecker.countInversionPairs([5, 5, 5, 5, 5]) == 0