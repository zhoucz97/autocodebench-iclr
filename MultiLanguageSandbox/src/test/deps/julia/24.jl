function check_matrix_parity(matrix)
"""
Check if a given n x n matrix of 0s and 1s has an even number of 1s in each row and column. 
If not, determine if changing one element can achieve this condition.

This function returns "OK" if the matrix already meets the condition. 
If changing one element can meet the condition, it returns the row and column index of that element. 
If neither is possible, it returns "Corrupt".

# Examples
>>> check_matrix_parity([1 0 1 0; 0 0 0 0; 1 1 1 1; 0 1 0 1])
"OK"
>>> check_matrix_parity([1 1 1 0; 0 0 0 0; 1 1 1 1; 0 1 0 1])
(1, 4)
>>> check_matrix_parity([1 1 1 1; 1 1 1 1; 1 1 1 1; 1 1 1 1])
"Corrupt"
"""
    n = size(matrix, 1)
    
    # Check if all rows and columns have even parity
    row_parity = [sum(row) % 2 for row in eachrow(matrix)]
    col_parity = [sum(col) % 2 for col in eachcol(matrix)]
    
    if all(row_parity .== 0) && all(col_parity .== 0)
        return "OK"
    end
    
    # Count how many rows and columns have odd parity
    odd_rows = sum(row_parity)
    odd_cols = sum(col_parity)
    
    # If exactly one row and one column have odd parity, flipping that element will fix it
    if odd_rows == 1 && odd_cols == 1
        # Find the position where both row and column have odd parity
        row_idx = findfirst(==(1), row_parity)
        col_idx = findfirst(==(1), col_parity)
        return (row_idx, col_idx)
    else
        return "Corrupt"
    end
end
@assert check_matrix_parity([1 0 1 0; 0 0 0 0; 1 1 1 1; 0 1 0 1]) == "OK"
@assert check_matrix_parity([1 1 1 0; 0 0 0 0; 1 1 1 1; 0 1 0 1]) == (1, 2)
@assert check_matrix_parity([1 1 1 1; 1 1 1 1; 1 1 1 1; 1 1 1 1]) == "OK"
@assert check_matrix_parity([1 0 1 0; 1 0 1 0; 1 0 1 0; 0 1 0 1]) == "Corrupt"