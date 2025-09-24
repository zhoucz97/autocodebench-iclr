function longest_plateau(numbers::Vector{Int}) :: Int
    """
    Finds the length of the longest contiguous sequence of equal values in a sorted array. 
    A plateau is defined as a sequence of consecutive elements with the same value that cannot be extended. 
    For example, in the array [1, 2, 2, 3, 3, 3, 4, 5, 5, 6], the longest plateau is [3, 3, 3], with a length of 3.

    Examples:
    >>> longest_plateau([1, 2, 2, 3, 3, 3, 4, 5, 5, 6])
    3
    >>> longest_plateau([1, 1, 2, 2, 3, 3, 4, 4, 4, 4])
    4
    """
    if isempty(numbers)
        return 0
    end
    
    max_length = 1
    current_length = 1
    
    for i in 2:length(numbers)
        if numbers[i] == numbers[i-1]
            current_length += 1
            if current_length > max_length
                max_length = current_length
            end
        else
            current_length = 1
        end
    end
    
    return max_length
end
@assert longest_plateau([1, 2, 2, 3, 3, 3, 4, 5, 5, 6]) == 3
@assert longest_plateau([1, 1, 2, 2, 3, 3, 4, 4, 4, 4]) == 4
@assert longest_plateau([1, 1, 1, 1, 1, 1]) == 6
@assert longest_plateau([1, 2, 3, 4, 5, 6]) == 1