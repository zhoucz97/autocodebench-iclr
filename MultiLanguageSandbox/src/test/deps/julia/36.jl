function find_max_and_lines(input::Vector{Vector{Int}})
    """
    Given a 2D array of unsigned integers, find the maximum integer and the line numbers (1-based) where it appears. If the maximum integer appears in multiple lines, list the line numbers in ascending order and separate them with commas.

    Parameters:
    - input (Vector{Vector{Int}}): A 2D array of unsigned integers where each row represents a line.

    Returns:
    - Tuple{Int, Vector{Int}}: A tuple containing the maximum integer and a vector of line numbers where it appears.

    Example:
    >>> find_max_and_lines([[1, 3, 5, 23, 6, 8, 14], [20, 22, 13, 4, 16], [23, 12, 17, 22], [2, 6, 10, 9, 3, 6], [22, 21, 20, 8, 10], [22, 1, 23, 6, 8, 19, 23]])
    (23, [1, 3, 6])
    """
    # Initialize variables to store the maximum value and its line numbers
    max_value = -Inf
    line_numbers = Int[]

    # Iterate over each line in the input
    for (line_number, line) in enumerate(input)
        # Find the maximum value in the current line
        current_max = maximum(line)
        
        # Update the global maximum and line numbers if necessary
        if current_max > max_value
            max_value = current_max
            line_numbers = [line_number]
        elseif current_max == max_value
            push!(line_numbers, line_number)
        end
    end

    # Return the maximum value and the sorted line numbers
    return (max_value, sort(line_numbers))
end
# Testcase 1
@assert find_max_and_lines([[1, 3, 5, 23, 6, 8, 14], [20, 22, 13, 4, 16], [23, 12, 17, 22], [2, 6, 10, 9, 3, 6], [22, 21, 20, 8, 10], [22, 1, 23, 6, 8, 19, 23]]) == (23, [1, 3, 6])

# Testcase 2
@assert find_max_and_lines([[5, 5, 5, 5], [1, 2, 3], [4, 4, 4]]) == (5, [1])

# Testcase 3
@assert find_max_and_lines([[10, 20, 30], [40, 50], [60, 70, 80, 90]]) == (90, [3])

# Testcase 4
@assert find_max_and_lines([[1]]) == (1, [1])