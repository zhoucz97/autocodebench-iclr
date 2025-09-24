function longest_consecutive_sequence(numbers::Vector{Int}) :: Int
    """
    Calculate the length of the longest consecutive sequence in a given list of integers.

    The consecutive sequence is a series of numbers where each number follows the previous one without any gaps. The sequence can start with any number and the numbers do not need to be in order initially.

    # Examples
    - `longest_consecutive_sequence([1, 5, 6, 2, 3, 4, 5, 6, 8, 9])` should return `5` because the longest consecutive sequence is `[1, 2, 3, 4, 5,6]`.
    - `longest_consecutive_sequence([10, 4, 20, 1, 3, 2])` should return `4` because the longest consecutive sequence is `[1, 2, 3, 4]`.
    """
    if isempty(numbers)
        return 0
    end
    
    num_set = Set(numbers)
    max_length = 0
    
    for num in num_set
        # Check if it's the start of a sequence
        if !(num - 1 in num_set)
            current_num = num
            current_length = 1
            
            while current_num + 1 in num_set
                current_num += 1
                current_length += 1
            end
            
            max_length = max(max_length, current_length)
        end
    end
    
    return max_length
end
@assert longest_consecutive_sequence([1, 5, 6, 2, 3, 4, 5, 6, 8, 9]) == 6
@assert longest_consecutive_sequence([10, 4, 20, 1, 3, 2]) == 4
@assert longest_consecutive_sequence([100, 4, 200, 1, 3, 2]) == 4
@assert longest_consecutive_sequence([5, 4, 2, 1]) == 2