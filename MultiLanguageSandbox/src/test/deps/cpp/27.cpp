#include <cctype>
#include <cassert>
#include <cstdio>
/**
 * Transforms the case of a given letter.
 * 
 * If the input is a lowercase letter, it returns the uppercase version,
 * and if the input is an uppercase letter, it returns the lowercase version.
 * Non-alphabetic characters are returned unchanged.
 *
 * Usage:
 *   char result = transform_letter_case('b'); // result will be 'B'
 *   char result = transform_letter_case('B'); // result will be 'b'
 *
 * @param letter The input letter to be transformed.
 * @return The letter with its case reversed if it's alphabetic, otherwise the original letter.
 */
// for islower, isupper, tolower, toupper

char transform_letter_case(char letter) {
    if (islower(letter)) {
        return toupper(letter);
    } else if (isupper(letter)) {
        return tolower(letter);
    }
    return letter;
}
int main() {
    assert(transform_letter_case('b') == 'B');
    assert(transform_letter_case('B') == 'b');
    // Additional test cases
    assert(transform_letter_case('z') == 'Z');
    assert(transform_letter_case('Z') == 'z');
    assert(transform_letter_case('m') == 'M');
    assert(transform_letter_case('M') == 'm');
    assert(transform_letter_case('1') == '1'); // Non-alphabetic, should be unchanged
    assert(transform_letter_case('!') == '!'); // Non-alphabetic, should be unchanged

    // Uncomment the following line to print a success message (optional)
    
    return 0;
}