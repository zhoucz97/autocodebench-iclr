function increment_digit_array(digits::Vector{Int}) :: Vector{Int}
    """
    Increment a non-empty array of digits by one. Each element in the array represents a single digit of a non-negative integer, with the most significant digit at the start of the array. The integer does not start with zero except for the integer 0 itself.
    
    # Examples
    >>> increment_digit_array([1, 9, 9])
    [2, 0, 0]
    
    >>> increment_digit_array([9, 9])
    [1, 0, 0]
    
    >>> increment_digit_array([1, 2, 3])
    [1, 2, 4]
    """
    n = length(digits)
    carry = 1  # Start with a carry of 1 to represent the increment
    
    for i in n:-1:1
        digits[i] += carry
        if digits[i] < 10
            carry = 0
            break
        else
            digits[i] = 0
            carry = 1
        end
    end
    
    if carry == 1
        pushfirst!(digits, 1)
    end
    
    return digits
end
@assert increment_digit_array([1, 9, 9]) == [2, 0, 0]
@assert increment_digit_array([9, 9]) == [1, 0, 0]
@assert increment_digit_array([2, 3, 4]) == [2, 3, 5]
@assert increment_digit_array([0]) == [1]
@assert increment_digit_array([1, 2, 3]) == [1, 2, 4]