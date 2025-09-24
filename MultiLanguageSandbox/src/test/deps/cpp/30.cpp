
#include <cassert>
#include <cstring>
#include <cstdio>
/**
 * Checks if a three-character string has exactly two characters that are the same.
 * 
 * @param s A three-character string to be checked.
 * 
 * @return Returns "Yes" if the input has exactly two equal characters, otherwise "No".
 * Examples:
 *   >>> check_two_equal_digits("112")
 *   "Yes"
 *   >>> check_two_equal_digits("123")
 *   "No"
 */
const char* check_two_equal_digits(const char s[4]) {
    // Check all possible pairs of equal characters
    if ((s[0] == s[1] && s[0] != s[2]) || 
        (s[0] == s[2] && s[0] != s[1]) || 
        (s[1] == s[2] && s[1] != s[0])) {
        return "Yes";
    } else {
        return "No";
    }
}
int main() {
    // Testing the logic with various cases and using string comparison for assert.
    assert(strcmp(check_two_equal_digits("112"), "Yes") == 0);
    assert(strcmp(check_two_equal_digits("123"), "No") == 0);
    assert(strcmp(check_two_equal_digits("232"), "Yes") == 0);
    assert(strcmp(check_two_equal_digits("444"), "No") == 0); // All three characters are equal, not two.
    assert(strcmp(check_two_equal_digits("121"), "Yes") == 0);

    // Additional test cases to cover more scenarios
    assert(strcmp(check_two_equal_digits("787"), "Yes") == 0);
    assert(strcmp(check_two_equal_digits("999"), "No") == 0); // All three characters are equal
    assert(strcmp(check_two_equal_digits("890"), "No") == 0);
    assert(strcmp(check_two_equal_digits("556"), "Yes") == 0);
    assert(strcmp(check_two_equal_digits("353"), "Yes") == 0); // No two characters are equal

    
    return 0;
}