function count_grass_clusters(field::Vector{String})
    """
    Count the number of grass clusters in a given field.

    Given a field represented as an R x C grid with characters '#' representing grass and '.' representing empty spaces, calculate the number of grass clusters. A grass cluster consists of one or more adjacent grass patches sharing at least one side.

    Parameters:
    - R (Int): The number of rows in the field.
    - C (Int): The number of columns in the field.
    - field (Vector{String}): A 2D array of characters representing the field.

    Returns:
    - Int: The number of grass clusters in the field.

    Example:
    >>> count_grass_clusters(["#....#", "..#...", "..#..#", "...##.", "#....#"])
    7
    >>> count_grass_clusters([".#....", "..#...", "..#..#", "...##.", ".#...."])
    5
    """
    R = length(field)
    if R == 0
        return 0
    end
    C = length(field[1])
    
    # Convert the field into a matrix for easier access
    grid = [collect(row) for row in field]
    
    visited = falses(R, C)
    clusters = 0
    
    for i in 1:R
        for j in 1:C
            if grid[i][j] == '#' && !visited[i, j]
                # Start DFS to mark all connected '#'
                stack = [(i, j)]
                visited[i, j] = true
                while !isempty(stack)
                    x, y = pop!(stack)
                    # Check four directions
                    for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
                        nx, ny = x + dx, y + dy
                        if 1 <= nx <= R && 1 <= ny <= C && grid[nx][ny] == '#' && !visited[nx, ny]
                            visited[nx, ny] = true
                            push!(stack, (nx, ny))
                        end
                    end
                end
                clusters += 1
            end
        end
    end
    
    return clusters
end
@assert count_grass_clusters(["#....#", "..#...", "..#..#", "...##.", "#....#"]) == 7
@assert count_grass_clusters([".#....", "..#...", "..#..#", "...##.", ".#...."]) == 5
@assert count_grass_clusters(["#..#.", ".##.#", "....#", ".#.#."]) == 6
@assert count_grass_clusters(["###", "###", "###"]) == 1