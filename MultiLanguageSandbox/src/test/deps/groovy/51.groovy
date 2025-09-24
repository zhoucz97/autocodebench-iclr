
class Checker{
    static int totalSmokedCigarettes(int n, int k) {
        """
        Calculates the total number of cigarettes Peter can smoke given n initial cigarettes
        and k butts needed to exchange for one new cigarette.

        Parameters:
        n - the initial number of cigarettes Peter has.
        k - the number of cigarette butts needed to exchange for one new cigarette.

        Returns:
        The total number of cigarettes Peter can smoke.

        Examples:
        >>> total_smoked_cigarettes(4, 3)
        5
        >>> total_smoked_cigarettes(10, 3)
        14
        """

    int total = n
    int butts = n
    
    while (butts >= k) {
        int newCigs = butts / k
        total += newCigs
        butts = butts % k + newCigs
    }
    
    return total
}
}
def checker = new Checker()
// Test cases
assert checker.totalSmokedCigarettes(4, 3) == 5
assert checker.totalSmokedCigarettes(10, 3) == 14
assert checker.totalSmokedCigarettes(5, 5) == 6
assert checker.totalSmokedCigarettes(20, 4) == 26
assert checker.totalSmokedCigarettes(1, 2) == 1 // Edge case, not enough to exchange
assert checker.totalSmokedCigarettes(100, 5) == 124
assert checker.totalSmokedCigarettes(0, 3) == 0 // Edge case, starts with no cigarettes