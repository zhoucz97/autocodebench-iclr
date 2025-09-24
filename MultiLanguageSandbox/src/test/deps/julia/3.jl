function is_palindrome(number::Int) :: Bool

"""
Check if the given integer is a palindrome. A palindrome is a number that reads the same backward as forward.

Examples:
>>> is_palindrome(121)
true
>>> is_palindrome(-121)
false
>>> is_palindrome(10)
false
"""
    # Negative numbers can't be palindromes
    if number < 0
        return false
    end
    
    original = number
    reversed = 0
    
    while number > 0
        digit = number % 10
        reversed = reversed * 10 + digit
        number ÷= 10
    end
    
    return original == reversed
end
# Test cases
@assert is_palindrome(121) == true
@assert is_palindrome(-121) == false
@assert is_palindrome(10) == false
@assert is_palindrome(12321) == true
@assert is_palindrome(123321) == true
@assert is_palindrome(123456) == false
@assert is_palindrome(0) == true