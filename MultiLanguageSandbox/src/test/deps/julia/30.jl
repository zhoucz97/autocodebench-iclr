function calculate_mine_proximity(grid::Array{Char,2}) :: Array{Char,2}
    """
    This function takes a grid representing a Minesweeper field and calculates the number
    of adjacent mines for each non-mine cell. The grid is a 2D array of characters, where
    '*' represents a mine and '?' represents an unknown cell. The function returns a new
    grid where each '?' is replaced with the count of adjacent mines (including diagonals).

    Example:
    >>> calculate_mine_proximity(['*' '?' '?'; '?' '?' '?'; '?' '*' '?'])
    ['*' '1' '0'; '2' '2' '1'; '1' '*' '1']

    >>> calculate_mine_proximity(['*' '*' '?'; '?' '?' '?'; '?' '?' '*'])
    ['*' '*' '1'; '2' '4' '2'; '0' '2' '*']
    """
    rows, cols = size(grid)
    result = copy(grid)
    
    for i in 1:rows
        for j in 1:cols
            if grid[i,j] == '?'
                # Count adjacent mines (including diagonals)
                count = 0
                for di in -1:1
                    for dj in -1:1
                        ni, nj = i + di, j + dj
                        if 1 ≤ ni ≤ rows && 1 ≤ nj ≤ cols && grid[ni,nj] == '*'
                            count += 1
                        end
                    end
                end
                result[i,j] = Char('0' + count)
            end
        end
    end
    
    return result
end
@assert calculate_mine_proximity(['*' '?' '?'; '?' '?' '?'; '?' '*' '?']) == ['*' '1' '0'; '2' '2' '1'; '1' '*' '1']
@assert calculate_mine_proximity(['*' '*' '?'; '?' '?' '?'; '?' '?' '*']) == ['*' '*' '1'; '2' '3' '2'; '0' '1' '*']
@assert calculate_mine_proximity(['?' '?' '?'; '?' '*' '?'; '?' '?' '?']) == ['1' '1' '1'; '1' '*' '1'; '1' '1' '1']