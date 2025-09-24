function fill_snake_matrix(n::Int) :: Matrix{Int}
    """
    Generate a snake-filled matrix of size n x n.

    Given a positive integer n, create an n x n matrix and fill it in a snake-like pattern.
    Starting from the top-left corner, populate the matrix with integers from 1 to n^2 in a zigzag manner.
    - Fill the odd-numbered diagonals (left-bottom to right-top) from left to right.
    - Fill the even-numbered diagonals (right-top to left-bottom) from top to bottom.

    Example:
    >>> fill_snake_matrix(4)
    4×4 Matrix{Int64}:
    1  2  6  7
    3  5  8  13
    4  9  12 14
    10 11 15 16
    """
    matrix = zeros(Int, n, n)
    counter = 1
    
    for s in 0:(2n-2)
        if s % 2 == 0
            # Even diagonals: top to bottom (i increases, j decreases)
            i = min(s, n-1)
            j = s - i
            while i >= 0 && j < n
                matrix[i+1, j+1] = counter
                counter += 1
                i -= 1
                j += 1
            end
        else
            # Odd diagonals: left to right (i decreases, j increases)
            j = min(s, n-1)
            i = s - j
            while j >= 0 && i < n
                matrix[i+1, j+1] = counter
                counter += 1
                i += 1
                j -= 1
            end
        end
    end
    
    return matrix
end
@assert fill_snake_matrix(4) == [1 2 6 7; 3 5 8 13; 4 9 12 14; 10 11 15 16]
@assert fill_snake_matrix(3) == [1 2 6; 3 5 7; 4 8 9]
@assert fill_snake_matrix(1) == [1;;]