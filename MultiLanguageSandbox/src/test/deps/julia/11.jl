function integer_square_root(x::Int) :: Int
"""
Calculate the integer part of the square root of a given non-negative integer `x`.
The function returns the largest integer `y` such that `y*y <= x`.
The calculation is done without using any built-in exponent functions or operators.

# Examples
```
julia> integer_square_root(9)
3

julia> integer_square_root(15)
3

julia> integer_square_root(26)
5
"""
    if x < 0
        throw(DomainError("Input must be a non-negative integer"))
    elseif x == 0 || x == 1
        return x
    end
    
    low = 1
    high = x
    result = 0
    
    while low <= high
        mid = (low + high) ÷ 2
        mid_squared = mid * mid
        
        if mid_squared == x
            return mid
        elseif mid_squared < x
            low = mid + 1
            result = mid
        else
            high = mid - 1
        end
    end
    
    return result
end
@assert integer_square_root(9) == 3
@assert integer_square_root(15) == 3
@assert integer_square_root(26) == 5
@assert integer_square_root(1) == 1
@assert integer_square_root(0) == 0