
class UglyNumberFinder {
    static int findNthUglyNumber(int n) {
        """
        Find the nth ugly number.
        An ugly number is a positive integer whose prime factors are limited to 2, 3, and 5.
        For example, the first 10 ugly numbers are [1, 2, 3, 4, 5, 6, 8, 9, 10, 12].
        >>> findNthUglyNumber(10)
        12
        >>> findNthUglyNumber(1)
        1
        """

    if (n <= 0) return 0
    
    int[] uglyNumbers = new int[n]
    uglyNumbers[0] = 1
    
    int i2 = 0, i3 = 0, i5 = 0
    
    for (int i = 1; i < n; i++) {
        int nextMultipleOf2 = uglyNumbers[i2] * 2
        int nextMultipleOf3 = uglyNumbers[i3] * 3
        int nextMultipleOf5 = uglyNumbers[i5] * 5
        
        int nextUgly = Math.min(nextMultipleOf2, Math.min(nextMultipleOf3, nextMultipleOf5))
        uglyNumbers[i] = nextUgly
        
        if (nextUgly == nextMultipleOf2) i2++
        if (nextUgly == nextMultipleOf3) i3++
        if (nextUgly == nextMultipleOf5) i5++
    }
    
    return uglyNumbers[n - 1]
}
}
// Test cases
def uglyNumberFinder = new UglyNumberFinder()
assert uglyNumberFinder.findNthUglyNumber(10) == 12
assert uglyNumberFinder.findNthUglyNumber(1) == 1
assert uglyNumberFinder.findNthUglyNumber(15) == 24
assert uglyNumberFinder.findNthUglyNumber(20) == 36
assert uglyNumberFinder.findNthUglyNumber(30) == 80
assert uglyNumberFinder.findNthUglyNumber(40) == 144
assert uglyNumberFinder.findNthUglyNumber(50) == 243
assert uglyNumberFinder.findNthUglyNumber(60) == 384
assert uglyNumberFinder.findNthUglyNumber(200) == 16200