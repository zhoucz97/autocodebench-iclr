function sum_natural_numbers(x::Int, y::Int) :: Int
    """
    Calculate the sum of two given natural numbers.
    
    This function takes two integers and returns their sum.
    
    # Examples
    >>> sum_natural_numbers(10, 20)
    30
    
    >>> sum_natural_numbers(5, 15)
    20
    """
    return x + y
end
@assert sum_natural_numbers(10, 20) == 30
@assert sum_natural_numbers(5, 15) == 20
@assert sum_natural_numbers(0, 0) == 0
@assert sum_natural_numbers(-5, 10) == 5