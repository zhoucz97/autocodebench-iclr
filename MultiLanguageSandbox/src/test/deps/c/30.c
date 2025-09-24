#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Checks if a three-character string has exactly two characters that are the same.
​
Args:
    s (char array): The three-character string to be checked.
​
Returns:
    str: Returns 'Yes' if the input has exactly two equal characters, otherwise 'No'.
	>>> check_two_equal_digits(112)
	Yes
*/
const char* check_two_equal_digits(const char s[4]) {
    // Check if the first and second characters are equal
    if (s[0] == s[1] && s[1] != s[2]) {
        return "Yes";
    }
    // Check if the first and third characters are equal
    else if (s[0] == s[2] && s[0] != s[1]) {
        return "Yes";
    }
    // Check if the second and third characters are equal
    else if (s[1] == s[2] && s[0] != s[1]) {
        return "Yes";
    }
    // If none of the above conditions are met, return "No"
    else {
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