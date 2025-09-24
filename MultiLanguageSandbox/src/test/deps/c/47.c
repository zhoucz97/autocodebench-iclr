#include <stdbool.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Determine if a given string is a palindrome. A palindrome is a word, phrase, number, or other sequence of characters that reads the same forward and backward (ignoring spaces, punctuation, and capitalization).
    >>> isPalindrome("racecar")
    true
    >>> isPalindrome("hello")
    false
*/
bool isPalindrome(const char* str) {
    if (str == NULL) {
        return false;
    }
    
    int left = 0;
    int right = strlen(str) - 1;
    
    while (left < right) {
        // Skip non-alphanumeric characters from the left
        while (left < right && !isalnum(str[left])) {
            left++;
        }
        // Skip non-alphanumeric characters from the right
        while (left < right && !isalnum(str[right])) {
            right--;
        }
        
        // Compare characters (case-insensitive)
        if (tolower(str[left]) != tolower(str[right])) {
            return false;
        }
        
        left++;
        right--;
    }
    
    return true;
}
int main()
{
    assert(isPalindrome("A man a plan a canal Panama") == true);
    assert(isPalindrome("No lemon, no melon") == true);
    assert(isPalindrome("Was it a car or a cat I saw") == true);
    assert(isPalindrome("Madam, in Eden, I'm Adam") == true);
    assert(isPalindrome("Never odd or even") == true);
    assert(isPalindrome("Eva, can I see bees in a cave") == true);
    assert(isPalindrome("hello") == false);
    assert(isPalindrome("GitHub") == false);
    assert(isPalindrome("programming") == false);
    
    return 0;
}