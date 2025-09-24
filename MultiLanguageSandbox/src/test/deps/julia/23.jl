function swap_matrix_rows(matrix::Array{Int,2}, row1::Int, row2::Int) :: Array{Int,2}
    """
    Swap two rows in a given 2D integer matrix.
    
    This function takes a 2D matrix of integers and swaps the elements of the specified rows.
    Note: Row indices are 1-based.
    
    # Examples
    >>> swap_matrix_rows([1 2 3; 4 5 6; 7 8 9], 1, 3)
    3×3 Array{Int64,2}:
     7  8  9
     4  5  6
     1  2  3
    
    >>> swap_matrix_rows([10 20; 30 40], 1, 2)
    2×2 Array{Int64,2}:
     30  40
     10  20
    """
    # Create a copy of the original matrix to avoid modifying it directly
    new_matrix = copy(matrix)
    
    # Swap the rows
    new_matrix[[row1, row2], :] = new_matrix[[row2, row1], :]
    
    return new_matrix
end
@assert swap_matrix_rows([1 2 3; 4 5 6; 7 8 9], 1, 3) == [7 8 9; 4 5 6; 1 2 3]
@assert swap_matrix_rows([10 20; 30 40], 1, 2) == [30 40; 10 20]
@assert swap_matrix_rows([1 2; 3 4; 5 6; 7 8; 9 10], 2, 5) == [1 2; 9 10; 5 6; 7 8; 3 4]
@assert swap_matrix_rows([1 1 1; 2 2 2; 3 3 3], 2, 3) == [1 1 1; 3 3 3; 2 2 2]