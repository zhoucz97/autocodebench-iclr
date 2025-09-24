function reduce_matrix(matrix)
    """
    Reduces a given square matrix through a series of row and column zeroing operations, followed by element removal, until it becomes a 1x1 matrix. In each step, the matrix is first row-zeroed by subtracting the minimum value of each row from all elements in that row. Then, it is column-zeroed by subtracting the minimum value of each column from all elements in that column. After each zeroing process, the second row and second column are removed. The function returns the values that were in the second row and second column before every zeroing operations.

    # Examples
    >>> reduce_matrix([1 2 3; 2 3 4; 3 4 5])
    [3, 0]

    """
    # Initialize an array to store the values from the second row and column
    result = []
    
    while size(matrix, 1) > 1 && size(matrix, 2) > 1
        # Store the values from the second row and column
        push!(result, matrix[2, 2])
        
        # Row zeroing: subtract the minimum of each row from all elements in that row
        for i in 1:size(matrix, 1)
            min_row = minimum(matrix[i, :])
            matrix[i, :] .-= min_row
        end
        
        # Column zeroing: subtract the minimum of each column from all elements in that column
        for j in 1:size(matrix, 2)
            min_col = minimum(matrix[:, j])
            matrix[:, j] .-= min_col
        end
        
        # Remove the second row and second column
        matrix = [matrix[1, 1] matrix[1, 3:end]; matrix[3:end, 1] matrix[3:end, 3:end]]
    end
    
    return result
end

# Example usage:
# reduce_matrix([1 2 3; 2 3 4; 3 4 5])
@assert reduce_matrix([1 2 3; 2 3 4; 3 4 5]) == [3, 0]
@assert reduce_matrix([4 5 6; 5 6 7; 6 7 8]) == [6, 0]
@assert reduce_matrix([2 4 6; 4 6 8; 6 8 10]) == [6, 0]
@assert reduce_matrix([1 1 1; 1 1 1; 1 1 1]) == [1, 0]