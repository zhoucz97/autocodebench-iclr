#include <cctype>
#include <cassert>
#include <cctype> // For isalnum and tolower
#include <cstring> // For strlen
/**
 * Determine if a given string is a palindrome. A palindrome is a word, phrase,
 * number, or other sequence of characters that reads the same forward and
 * backward (ignoring spaces, punctuation, and capitalization).
 * Examples:
 *   >>> isPalindrome("racecar")
 *   true
 *   >>> isPalindrome("hello")
 *   false
 */
// for tolower

bool isPalindrome(const char* str) {
    if (str == nullptr) return false;
    
    const char* left = str;
    const char* right = str;
    
    // Move right pointer to the end of the string
    while (*right != '\0') {
        right++;
    }
    right--; // Point to the last character
    
    while (left < right) {
        // Skip non-alphanumeric characters from the left
        while (left < right && !isalnum(*left)) {
            left++;
        }
        // Skip non-alphanumeric characters from the right
        while (left < right && !isalnum(*right)) {
            right--;
        }
        
        // Compare characters (case-insensitive)
        if (tolower(*left) != tolower(*right)) {
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