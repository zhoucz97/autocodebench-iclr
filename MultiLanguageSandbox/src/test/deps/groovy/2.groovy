
boolean isPalindrome(int x) {
"""
    Check if the given integer is a palindrome.
    A palindrome is a number that reads the same forward as backward.
    >>> isPalindrome(121)
    true
    >>> isPalindrome(-12321)
    false
    >>> isPalindrome(10)
    false
"""    

    // Negative numbers cannot be palindromes
    if (x < 0) {
        return false
    }
    
    // Convert the number to a string for easy reversal
    String str = x.toString()
    String reversedStr = str.reverse()
    
    // Compare the original string with the reversed string
    return str == reversedStr
}
assert isPalindrome(121) == true
assert isPalindrome(-12321) == false
assert isPalindrome(10) == false
assert isPalindrome(12321) == true
assert isPalindrome(98789) == true
assert isPalindrome(12345) == false