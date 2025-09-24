

def count_partitions(n: int, k: int, x: int) -> int:
    """
    Count the number of ways to partition an integer n into k parts,
    where each part is at least x and order of parts does not matter.

    Parameters:
    n (int): The integer to be partitioned.
    k (int): The number of parts to divide n into.
    x (int): The minimum value for each part.

    Returns:
    int: The number of distinct partitioning ways.

    Examples:
    >>> count_partitions(7, 3, 1)
    4
    >>> count_partitions(6, 2, 1)
    3
    """
    # Check if it's possible to partition n into k parts each >= x
    if n < k * x:
        return 0
    
    # Transform the problem: subtract x from each part, so now we need to partition (n - k*x) into k parts >=0
    m = n - k * x
    
    # Now, the problem is to partition m into k parts where each part is >=0 (order doesn't matter)
    # This is equivalent to finding the number of combinations with repetition
    
    # Initialize a DP table where dp[i][j] represents the number of ways to partition i into j parts (each >=0)
    dp = [[0] * (k + 1) for _ in range(m + 1)]
    
    # Base case: there's one way to partition 0 into 0 parts
    dp[0][0] = 1
    
    for i in range(1, m + 1):
        for j in range(1, k + 1):
            # The number of ways is the sum of:
            # 1. partitions of i-1 into j-1 parts (adding a new part of 1)
            # 2. partitions of i-j into j parts (adding 1 to each of the j parts)
            if i - 1 >= 0:
                dp[i][j] += dp[i - 1][j - 1]
            if i - j >= 0:
                dp[i][j] += dp[i - j][j]
    
    return dp[m][k]
def test_count_partitions():
    # Test case 1: Simple case
    result = count_partitions(7, 3, 1)
    assert result == 4, f"Expected 4, got {result}"

    # Test case 2: Another simple case
    result = count_partitions(6, 2, 1)
    assert result == 3, f"Expected 3, got {result}"

    # Test case 3: Partitioning into a larger number of parts
    result = count_partitions(8, 4, 1)
    assert result == 5, f"Expected 5, got {result}"

    print("All test cases passed successfully!")


# Call the test function to run the test cases
test_count_partitions()