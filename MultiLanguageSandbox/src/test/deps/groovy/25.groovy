
class SquareRootCalculator {
    static int calculateSquareRoot(int x) {
        """
        Calculate the integer part of the square root of a given non-negative integer.
        >>> calculateSquareRoot(4)
        2
        >>> calculateSquareRoot(8)
        2
        """

    if (x == 0 || x == 1) {
        return x
    }
    
    int left = 1
    int right = x
    int result = 0
    
    while (left <= right) {
        int mid = left + (right - left) / 2
        int square = mid * mid
        
        if (square == x) {
            return mid
        } else if (square < x) {
            left = mid + 1
            result = mid
        } else {
            right = mid - 1
        }
    }
    
    return result
}
}
// Test cases
def squareRootCalculator = new SquareRootCalculator()
assert squareRootCalculator.calculateSquareRoot(16) == 4
assert squareRootCalculator.calculateSquareRoot(36) == 6
assert squareRootCalculator.calculateSquareRoot(50) == 7
assert squareRootCalculator.calculateSquareRoot(110) == 10
assert squareRootCalculator.calculateSquareRoot(0) == 0