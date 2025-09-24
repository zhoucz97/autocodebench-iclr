
class SingleNumberFinder {
    static int findSingleNumber(List nums) {
        """
        Find the single number in the given list of numbers. All numbers appear twice except one.
        >>> findSingleNumber([2, 2, 1])
        1
        >>> findSingleNumber([4, 1, 2, 1, 2])
        4
        >>> findSingleNumber([1])
        1
        """

    int result = 0
    for (num in nums) {
        result ^= num
    }
    return result
}
}
// Test cases
def singleNumberFinder = new SingleNumberFinder()
assert singleNumberFinder.findSingleNumber([2, 2, 1, 3, 3]) == 1
assert singleNumberFinder.findSingleNumber([4, 1, 2, 1, 2, 3, 3]) == 4
assert singleNumberFinder.findSingleNumber([1, 2, 2]) == 1
assert singleNumberFinder.findSingleNumber([1, 1, 2, 2, 3]) == 3
assert singleNumberFinder.findSingleNumber([1, 2, 3, 4, 5, 4, 3, 2, 1]) == 5
assert singleNumberFinder.findSingleNumber([1, 1, 2, 2, 3, 3, 4]) == 4
assert singleNumberFinder.findSingleNumber([1]) == 1