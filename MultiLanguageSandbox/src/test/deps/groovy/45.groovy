
class EggDroppingPuzzle {
    static int minOperations(int k, int n) {
        """
        Given k identical eggs and a building with n floors, determine the minimum number of operations
        needed to find out the value of f (the critical floor) with the least number of drops.
        The critical floor f is defined as the highest floor from which an egg dropped will not break.
        If an egg breaks when dropped from floor x, then all eggs dropped from floors higher than x will also break.
        If an egg does not break when dropped from floor x, then all eggs dropped from floors lower than x will not break.
        >>> minOperations(1, 2)
        2
        >>> minOperations(2, 6)
        3
        >>> minOperations(3, 14)
        4
        """

    if (n == 0 || k == 0) return 0
    int m = 0
    while (true) {
        int sum = 0
        int term = 1
        for (int i = 1; i <= k; i++) {
            term = term * (m - i + 1) / i
            sum += term
            if (sum >= n) break
        }
        if (sum >= n) return m
        m++
    }
}
}
// Test cases
def eggDroppingPuzzle = new EggDroppingPuzzle()
assert eggDroppingPuzzle.minOperations(1, 10) == 10
assert eggDroppingPuzzle.minOperations(2, 10) == 4
assert eggDroppingPuzzle.minOperations(3, 10) == 4
assert eggDroppingPuzzle.minOperations(3, 100) == 9
assert eggDroppingPuzzle.minOperations(10, 100) == 7