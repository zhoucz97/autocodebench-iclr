function count_paths(grid_size::Int) :: Int
"""
Calculate the number of distinct paths from the top-left corner to the bottom-right corner of a 2D grid of a given size.
Each move is either one step to the right or one step down.
>>> count_paths(2)
2
>>> count_paths(3)
6
"""
    if grid_size <= 1
        return 1
    end
    n = grid_size - 1
    total_moves = 2 * n
    # Calculate the binomial coefficient C(total_moves, n)
    # Using the multiplicative formula to avoid large intermediate values
    result = 1
    for k in 1:n
        result = result * (total_moves - k + 1) ÷ k
    end
    return result
end
@assert count_paths(2) == 2 # 2x2 grid has 2 paths (Right-Down, Down-Right)
@assert count_paths(3) == 6 # 3x3 grid has 6 paths
@assert count_paths(4) == 20 # 4x4 grid has 20 paths
@assert count_paths(1) == 1 # 1x1 grid has only 1 path