
class PowerOfThreeChecker {
    static boolean isPowerOfThree(int n) {
        """
        Check if a given number is a power of three.
        >>> isPowerOfThree(27)
        True
        >>> isPowerOfThree(0)
        False
        >>> isPowerOfThree(9)
        True
        >>> isPowerOfThree(45)
        False
        """

    if (n <= 0) {
        return false
    }
    // The largest power of 3 that fits in a 32-bit signed integer is 3^19 = 1162261467
    // If 1162261467 is divisible by n, then n is a power of 3
    return 1162261467 % n == 0
}
}
def powerOfThreeChecker = new PowerOfThreeChecker()
assert powerOfThreeChecker.isPowerOfThree(27) == true
assert powerOfThreeChecker.isPowerOfThree(0) == false
assert powerOfThreeChecker.isPowerOfThree(9) == true
assert powerOfThreeChecker.isPowerOfThree(45) == false
assert powerOfThreeChecker.isPowerOfThree(1) == true
assert powerOfThreeChecker.isPowerOfThree(2) == false
assert powerOfThreeChecker.isPowerOfThree(81) == true
assert powerOfThreeChecker.isPowerOfThree(100) == false