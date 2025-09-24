

from typing import List, Tuple 
def max_herb_value(T: int, M: int, herbs: List[Tuple[int, int]]) -> int:
    """
    Calculate the maximum total value of herbs that can be picked within a given time limit.

    This function solves a variation of the 0/1 knapsack problem where the goal is to maximize
    the total value of herbs picked without exceeding the time limit. Each herb can be picked
    at most once and has an associated time cost and value.

    Args:
    - T (int): The total time available for picking herbs.
    - M (int): The number of different herbs in the cave.
    - herbs (List[Tuple[int, int]]): A list of tuples, where each tuple contains two integers
      representing the time required to pick the herb and the herb's value, respectively.

    Returns:
    - int: The maximum total value of herbs that can be picked within the time limit.

    Examples:
    - max_herb_value(70, 3, [(71, 100), (69, 1), (1, 2)]) -> 3
    - max_herb_value(5, 3, [(5, 10), (4, 7), (6, 12)]) -> 10
    """
    # Initialize a DP array where dp[j] represents the maximum value achievable with time j
    dp = [0] * (T + 1)
    
    for time, value in herbs:
        # Iterate from T down to time to avoid reusing the same herb multiple times
        for j in range(T, time - 1, -1):
            if dp[j - time] + value > dp[j]:
                dp[j] = dp[j - time] + value
    
    return dp[T]
def test_max_herb_value():
    # Test case 1: Given example in the problem statement
    T1, M1 = 70, 3
    herbs1 = [(71, 100), (69, 1), (1, 2)]
    expected1 = 3
    assert max_herb_value(T1, M1,
                          herbs1) == expected1, f"Test case 1 failed: expected {expected1}, got {max_herb_value(T1, M1, herbs1)}"

    # Test case 2: Multiple herbs can be picked within the time limit
    T2, M2 = 10, 4
    herbs2 = [(1, 10), (2, 15), (3, 20), (4, 30)]
    expected2 = 75  # We can pick herbs with time 1, 2 and 3 to get maximum value 10 + 15 + 20
    assert max_herb_value(T2, M2,
                          herbs2) == expected2, f"Test case 2 failed: expected {expected2}, got {max_herb_value(T2, M2, herbs2)}"

    # Test case 3: Only one herb can be picked due to time constraint
    T3, M3 = 5, 3
    herbs3 = [(5, 10), (4, 7), (6, 12)]
    expected3 = 10  # We can only pick the first herb
    assert max_herb_value(T3, M3,
                          herbs3) == expected3, f"Test case 3 failed: expected {expected3}, got {max_herb_value(T3, M3, herbs3)}"

    # If all tests pass, print this:
    print("All test cases passed!")
test_max_herb_value()