
class CloseElementsChecker {
    static boolean hasCloseElements(List<Integer> nums, int indexDiff, int valueDiff) {
        """
        Check if in given list of numbers, are any two numbers closer to each other than
        given threshold, and their indices are within the given index difference.
        >>> hasCloseElements([1, 2, 3, 1], 3, 0)
        True
        >>> hasCloseElements([1, 5, 9, 1, 5, 9], 2, 3)
        False
        """

    for (int i = 0; i < nums.size(); i++) {
        for (int j = i + 1; j < nums.size(); j++) {
            if (Math.abs(nums[i] - nums[j]) <= valueDiff && Math.abs(i - j) <= indexDiff) {
                return true
            }
        }
    }
    return false
}
}
def closeElementsChecker = new CloseElementsChecker()
assert closeElementsChecker.hasCloseElements([1, 2, 3, 1], 3, 0) == true
assert closeElementsChecker.hasCloseElements([1, 5, 9, 1, 5, 9], 2, 3) == false
assert closeElementsChecker.hasCloseElements([1, 2, 3, 4, 5, 6], 2, 1) == true
assert closeElementsChecker.hasCloseElements([1, 2, 3, 4, 5, 6], 1, 0) == false
assert closeElementsChecker.hasCloseElements([1, 2, 3, 4, 5, 6], 3, 2) == true
assert closeElementsChecker.hasCloseElements([1, 2, 3, 4, 5, 6], 2, 0) == false