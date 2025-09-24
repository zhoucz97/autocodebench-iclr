
class CloseElementsChecker {
    static boolean hasCloseElements(List<Double> numbers, double threshold) {
        """
        Check if in given list of numbers, are any two numbers closer to each other than
        given threshold.
        >>> has_close_elements([1.0, 2.0, 3.0], 0.5)
        False
        >>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)
        True
        """

    // Sort the list to easily find the closest elements
    def sortedNumbers = numbers.sort()
    
    // Iterate through the sorted list and check adjacent elements
    for (int i = 0; i < sortedNumbers.size() - 1; i++) {
        double current = sortedNumbers[i]
        double next = sortedNumbers[i + 1]
        
        // Check if the difference between current and next is less than the threshold
        if (Math.abs(next - current) < threshold) {
            return true
        }
    }
    
    // If no such pair is found, return false
    return false
}
}
// Test cases
def closeElementsChecker = new CloseElementsChecker()
assert closeElementsChecker.hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3) == true
assert closeElementsChecker.hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05) == false
assert closeElementsChecker.hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95) == true
assert closeElementsChecker.hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8) == false
assert closeElementsChecker.hasCloseElements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1) == true
assert closeElementsChecker.hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0) == true
assert closeElementsChecker.hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5) == false