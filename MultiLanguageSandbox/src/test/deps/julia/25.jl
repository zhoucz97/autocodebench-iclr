function find_saddle_point(matrix::Matrix{Int}) :: Union{Tuple{Int, Int, Int}, String}
"""
Find the saddle point in a given 5x5 matrix. A saddle point is an element which is the maximum in its row and minimum in its column.

- Parameters:
    - matrix: A 5x5 matrix of integers.

- Returns: 
    - A tuple (row, column, value) of the saddle point if found; otherwise, "not found".

Example:
    find_saddle_point([11 3 5 6 9; 12 4 7 8 10; 10 5 6 9 11; 8 6 4 7 2; 15 10 11 20 25]) 
    # should return (4, 1, 8)

"""
    rows, cols = size(matrix)
    
    for i in 1:rows
        for j in 1:cols
            current_value = matrix[i, j]
            
            # Check if current element is the maximum in its row
            is_row_max = all(current_value >= matrix[i, :])
            
            # Check if current element is the minimum in its column
            is_col_min = all(current_value <= matrix[:, j])
            
            if is_row_max && is_col_min
                return (i, j, current_value)
            end
        end
    end
    
    return "not found"
end
@assert find_saddle_point([11 3 5 6 9; 12 4 7 8 10; 10 5 6 9 11; 8 6 4 7 2; 15 10 11 20 25]) == (4, 1, 8)
@assert find_saddle_point([1 2 3 4 5; 6 7 8 9 10; 11 12 13 14 15; 16 17 18 19 20; 21 22 23 24 25]) == (1, 5, 5)
@assert find_saddle_point([10 20 30 40 50; 40 30 20 10 0; 50 40 30 20 10; 0 10 20 30 40; 30 20 10 0 50]) == "not found"