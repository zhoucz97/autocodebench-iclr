
class Solution {
    static int findNthDigit(int n) {
        """
        Find the nth digit in the infinite integer sequence [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...].
        >>> findNthDigit(3)
        3
        >>> findNthDigit(11)
        0
        """

    int length = 1; // The number of digits in the current range
    int count = 9;  // The count of numbers in the current range (e.g., 9 for 1-digit numbers)
    int start = 1;  // The first number in the current range (e.g., 1 for 1-digit numbers)
    
    // Find the range where the nth digit is located
    while (n > length * count) {
        n -= length * count;
        length += 1;
        count *= 10;
        start *= 10;
    }
    
    // Find the specific number that contains the nth digit
    int number = start + (n - 1) / length;
    
    // Find the specific digit in the number
    String numStr = number.toString();
    int digitIndex = (n - 1) % length;
    return Character.getNumericValue(numStr.charAt(digitIndex));
}
}
def solution = new Solution()
assert solution.findNthDigit(3) == 3
assert solution.findNthDigit(11) == 0
assert solution.findNthDigit(100) == 5
assert solution.findNthDigit(1000) == 3
assert solution.findNthDigit(10000) == 7
assert solution.findNthDigit(100000) == 2
assert solution.findNthDigit(1000000) == 1
assert solution.findNthDigit(10000000) == 7