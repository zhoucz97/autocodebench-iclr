


def max_energy(beads):
    """
    Calculate the maximum energy that can be released by merging beads on a necklace.

    The function takes a list of integers representing the energy beads on a necklace, where
    each bead has a head and a tail value. The head value of each bead must match the tail
    value of the next bead in the sequence. The necklace is circular, and merging two adjacent
    beads releases energy equal to the product of the head value of the first bead, the matching
    value, and the tail value of the second bead.

    To find the maximum energy release, the function considers all possible orders of merging beads
    and uses dynamic programming to compute the maximum energy obtainable.

    Args:
        beads: A list of integers where each integer represents the head value of a bead and
               the tail value of the previous bead. The tail value of the last bead is assumed
               to match the head value of the first bead due to the circular nature of the necklace.

    Returns:
        An integer representing the maximum energy that can be obtained by optimally merging all beads.

    Examples:
        >>> max_energy([2, 3, 5, 10])
        710
        >>> max_energy([1, 2, 3, 4])
        48
    """
    n = len(beads)
    if n == 1:
        return 0  # no merging possible
    
    max_total = 0
    
    # We need to consider each possible starting point in the circular necklace
    for start in range(n):
        # Create a linear array by breaking the circle at 'start'
        linear = []
        for i in range(n):
            pos = (start + i) % n
            linear.append(beads[pos])
        
        # Now apply dynamic programming on the linear array
        m = len(linear)
        dp = [[0] * m for _ in range(m)]
        
        for length in range(2, m + 1):  # length of the interval to merge
            for i in range(m - length + 1):
                j = i + length - 1
                dp[i][j] = 0
                for k in range(i, j):
                    # The energy is dp[i][k] + dp[k+1][j] + linear[i] * linear[k+1] * linear[j+1 mod m]
                    # But since it's linear, linear[j+1] is linear[(j+1)%m], but in linear array, it's linear[j+1] if j+1 < m, else linear[0]
                    # Wait, no: in the linear array, the next after j is j+1, but in the original circular, it's (start + (j+1)) mod n.
                    # But in the linear array, the tail of bead j is linear[j+1] if j+1 < m, else linear[0] (but in linear array, it's not there)
                    # So for the linear array, the tail of bead j is linear[j+1] if j+1 < m, else linear[0] (but linear array doesn't have that)
                    # So perhaps the linear array should be extended by one element to handle the circularity?
                    # Alternative approach: the linear array is beads[start], beads[(start+1)%n], ..., beads[(start+n-1)%n], and then the tail of the last bead is beads[start].
                    # So the linear array is of size n, and the tail of bead i is bead[i+1] for i < n-1, and bead[n-1]'s tail is bead[0].
                    # So in the DP, when merging i..j, the energy is linear[i] * linear[k+1] * linear[j]'s tail. But linear[j]'s tail is linear[j+1] if j+1 <n, else linear[0].
                    # So for the linear array constructed as above, the tail of bead j is linear[(j+1)%n] in the original array, but in the linear array, it's linear[j+1] if j+1 <n, else linear[0].
                    # But the linear array is of size n, so j ranges from 0 to n-1. So for j = n-1, linear[j+1] is out of bounds. So perhaps the linear array should be of size n+1, with linear[n] = linear[0].
                    # Let's adjust the linear array construction:
                    pass
        
        # Re-defining the linear array to handle the circularity properly
        # The linear array should be beads[start], beads[(start+1)%n], ..., beads[(start+n-1)%n], and then an additional bead[start] at the end to represent the circular connection.
        # So the linear array is of size n+1.
        linear = []
        for i in range(n + 1):
            pos = (start + i) % n
            linear.append(beads[pos])
        
        m = len(linear) - 1  # the actual beads are up to index m-1 (0..m-1), linear[m] is the tail of linear[m-1]
        dp = [[0] * m for _ in range(m)]
        
        for length in range(2, m + 1):
            for i in range(m - length + 1):
                j = i + length - 1
                dp[i][j] = 0
                for k in range(i, j):
                    # The energy is dp[i][k] + dp[k+1][j] + linear[i] * linear[k+1] * linear[j+1]
                    # Because linear[j+1] is the tail of bead j in the linear array (which is beads[(start + (j+1)) % n])
                    current = dp[i][k] + dp[k+1][j] + linear[i] * linear[k+1] * linear[j+1]
                    if current > dp[i][j]:
                        dp[i][j] = current
        
        current_max = dp[0][m - 1]
        if current_max > max_total:
            max_total = current_max
    
    return max_total
def test_max_energy():
    assert max_energy([2, 3, 5, 10]) == 710, "Testcase 1 failed"
    assert max_energy([1, 2, 3, 4]) == 80, "Testcase 2 failed"
    assert max_energy([4, 4, 4, 4]) == 192, "Testcase 3 failed"
    assert max_energy([30, 40, 50, 60]) == 372000, "Testcase 4 failed"
    print("All testcases passed")

# Call the test function
test_max_energy()