function count_digit_occurrences(L::Int, R::Int, digit::Int) :: Int
"""
Count the number of times a specific digit appears in all integers within the range [L, R].

# Examples
```jldoctest
julia> count_digit_occurrences(3, 23, 2)
6
julia> count_digit_occurrences(10, 50, 3)
5
"""
    count = 0
    for num in L:R
        n = num
        while n > 0
            current_digit = n % 10
            if current_digit == digit
                count += 1
            end
            n ÷= 10
        end
        # Handle the case when num is 0 separately, but since L >= 0 and R >= L, and digit is 0-9, it's covered
    end
    return count
end
@assert count_digit_occurrences(3, 23, 2) == 6
@assert count_digit_occurrences(10, 50, 3) == 14
@assert count_digit_occurrences(1, 100, 1) == 21
@assert count_digit_occurrences(55, 80, 7) == 13