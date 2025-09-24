

def calculate_arrangements(n, m, a) -> int:
    """
    Compute the number of ways to arrange m pots of flowers using up to n types,
    where the ith type can have at most a[i] pots, and the arrangement must be in
    increasing order of flower types.

    Args:
    - n (int): The number of flower types available.
    - m (int): The total number of flower pots to arrange.
    - a (list of int): A list where a[i] is the maximum number of pots for the ith type of flower.

    Returns:
    - int: The number of distinct arrangements modulo (10^6 + 7).

    Examples:
    - calculate_arrangements(2, 4, [3, 2]) returns 2.
    - calculate_arrangements(3, 3, [1, 2, 3]) returns 6.
    """
    # Initialize a DP table where dp[i][j] represents the number of ways to arrange j pots using first i types
    dp = [[0] * (m + 1) for _ in range(n + 1)]
    
    # Base case: 0 pots can be arranged in 1 way (using 0 of each type)
    for i in range(n + 1):
        dp[i][0] = 1
    
    for i in range(1, n + 1):
        max_pots = a[i - 1]
        for j in range(1, m + 1):
            # The number of ways is the sum of dp[i-1][j - k] for k from 0 to min(max_pots, j)
            total = 0
            max_k = min(max_pots, j)
            for k in range(0, max_k + 1):
                total += dp[i - 1][j - k]
                total %= MOD
            dp[i][j] = total
    
    return dp[n][m] % MOD
def test_calculate_arrangements():
    # Helper function to compare expected and actual results
    def assert_equal(actual, expected, message):
        assert actual == expected, message

    # Test Case 1
    n, m, a = 2, 4, [3, 2]
    expected = 2
    assert_equal(calculate_arrangements(n, m, a), expected, f"Test Case 1 failed: expected {expected}, got {calculate_arrangements(n, m, a)}")

    # Test Case 2
    n, m, a = 3, 3, [1, 2, 3]
    expected = 6
    assert_equal(calculate_arrangements(n, m, a), expected, f"Test Case 2 failed: expected {expected}, got {calculate_arrangements(n, m, a)}")

    # Test Case 3
    n, m, a = 1, 5, [5]
    expected = 1
    assert_equal(calculate_arrangements(n, m, a), expected, f"Test Case 3 failed: expected {expected}, got {calculate_arrangements(n, m, a)}")

    print("All tests passed!")

if __name__ == "__main__":
    test_calculate_arrangements()