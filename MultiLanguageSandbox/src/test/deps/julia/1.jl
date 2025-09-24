function has_close_elements(numbers::Vector{Float64}, threshold::Float64) :: Bool
    """
    Check if in given list of numbers, are any two numbers closer to each other than
    given threshold.
    >>> has_close_elements([1.0, 2.0, 3.0], 0.5)
    False
    >>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)
    True
    """
    # Sort the numbers to easily check adjacent elements
    sorted_numbers = sort(numbers)
    
    # Check all adjacent pairs in the sorted list
    for i in 1:length(sorted_numbers)-1
        if sorted_numbers[i+1] - sorted_numbers[i] < threshold
            return true
        end
    end
    
    return false
end
# Test cases
@assert has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3) == true
@assert has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05) == false
@assert has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95) == true
@assert has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8) == false
@assert has_close_elements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1) == true
@assert has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0) == true
@assert has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5) == false