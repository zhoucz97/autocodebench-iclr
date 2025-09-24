

from typing import List, Tuple

def max_value_on_paths(N: int, grid_values: List[Tuple[int, int, int]]) -> int:
    """
    Calculate the maximum sum of values collected on two paths in an N x N grid.

    This function utilizes dynamic programming to find two paths from the top-left corner to
    the bottom-right corner of the grid which maximize the sum of the values collected. Each
    value can be collected at most once, even if both paths pass through it.

    Args:
    - N (int): The size of the grid (N x N).
    - grid_values (List[Tuple[int, int, int]]): A list of tuples where each tuple contains
      the x-coordinate, y-coordinate, and value to be placed on the grid at that position.
      The list is terminated by a tuple with all zeros.

    Returns:
    - int: The maximum sum of values collected on the two paths.

    Examples:
    >>> max_value_on_paths(2, [(1, 2, 1), (2, 1, 2), (0, 0, 0)])
    3
    >>> max_value_on_paths(8, [
    ...     (2, 3, 13), (2, 6, 6), (3, 5, 7), (4, 4, 14),
    ...     (5, 2, 21), (5, 6, 4), (6, 3, 15), (7, 2, 14),
    ...     (0, 0, 0)])
    67
    """
    # Initialize the grid with zeros
    grid = [[0] * (N + 1) for _ in range(N + 1)]  # 1-based indexing
    
    # Populate the grid from the input list until a zero tuple is encountered
    for x, y, value in grid_values:
        if x == 0 and y == 0 and value == 0:
            break
        grid[x][y] = value
    
    # Initialize a DP table: dp[i1][j1][i2][j2] represents the max sum when first path is at (i1,j1) and second at (i2,j2)
    # But since N can be up to 8 in the example, a 4-dimensional array is manageable (8^4 = 4096 entries)
    # Using a dictionary or a 4D array. Here, we'll use a 4D array for simplicity.
    # The paths can be at most N steps right and N steps down, so coordinates are from 1 to N.
    dp = [[[[ -1 for _ in range(N+1) ] for __ in range(N+1) ] for ___ in range(N+1) ] for ____ in range(N+1)]
    
    # Base case: both start at (1,1)
    i1, j1 = 1, 1
    i2, j2 = 1, 1
    total = grid[i1][j1]
    # Since both are at (1,1), the value is added once.
    dp[i1][j1][i2][j2] = total
    
    # Directions: right (0,1) or down (1,0)
    directions = [(0, 1), (1, 0)]
    
    max_sum = 0
    
    from collections import deque
    queue = deque()
    queue.append((i1, j1, i2, j2))
    
    while queue:
        ci1, cj1, ci2, cj2 = queue.popleft()
        current_sum = dp[ci1][cj1][ci2][cj2]
        if ci1 == N and cj1 == N and ci2 == N and cj2 == N:
            if current_sum > max_sum:
                max_sum = current_sum
            continue
        # Generate all possible next steps for both paths
        for di1, dj1 in directions:
            ni1, nj1 = ci1 + di1, cj1 + dj1
            if 1 <= ni1 <= N and 1 <= nj1 <= N:
                for di2, dj2 in directions:
                    ni2, nj2 = ci2 + di2, cj2 + dj2
                    if 1 <= ni2 <= N and 1 <= nj2 <= N:
                        # Calculate the added value
                        if (ni1, nj1) == (ni2, nj2):
                            added = grid[ni1][nj1]
                        else:
                            added = grid[ni1][nj1] + grid[ni2][nj2]
                        new_sum = current_sum + added
                        if dp[ni1][nj1][ni2][nj2] < new_sum:
                            dp[ni1][nj1][ni2][nj2] = new_sum
                            queue.append((ni1, nj1, ni2, nj2))
    
    return max_sum
def test_max_value_on_paths():
    # Test case 1: Small grid with clear path
    assert max_value_on_paths(2, [(1, 2, 1), (2, 1, 2), (0, 0, 0)]) == 3
    
    # Test case 2: Example provided in the problem statement
    assert max_value_on_paths(8, [
        (2, 3, 13), (2, 6, 6), (3, 5, 7), (4, 4, 14),
        (5, 2, 21), (5, 6, 4), (6, 3, 15), (7, 2, 14),
        (0, 0, 0)]) == 67

    print("All test cases passed!")

# Run the test cases
test_max_value_on_paths()