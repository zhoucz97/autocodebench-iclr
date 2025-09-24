
class BulbSwitcher {
    static int numBulbsOn(int n) {
        """
        Given an integer n, which represents the number of rounds of switching bulbs,
        return the number of bulbs that are on after n rounds.

        Each bulb is initially off. The first round, all bulbs are turned on.
        Each subsequent round, every second bulb is toggled (i.e., turned off if it was on, and turned on if it was off).
        The nth round, only the bulb at position n is toggled.

        Example:
        >>> numBulbsOn(3)
        1
        >>> numBulbsOn(0)
        0
        >>> numBulbsOn(1)
        1
        """

    return (int) Math.sqrt(n)
}
}
def bulbSwitcher = new BulbSwitcher()
assert bulbSwitcher.numBulbsOn(3) == 1
assert bulbSwitcher.numBulbsOn(0) == 0
assert bulbSwitcher.numBulbsOn(1) == 1
assert bulbSwitcher.numBulbsOn(2) == 1
assert bulbSwitcher.numBulbsOn(4) == 2
assert bulbSwitcher.numBulbsOn(5) == 2
assert bulbSwitcher.numBulbsOn(6) == 2
assert bulbSwitcher.numBulbsOn(7) == 2
assert bulbSwitcher.numBulbsOn(8) == 2
assert bulbSwitcher.numBulbsOn(9) == 3
assert bulbSwitcher.numBulbsOn(10) == 3