#include <ctype.h>
#include <assert.h>
#include <stdio.h>
/*
Transforms the case of a given letter.
    Parameters:
- letter (char): The input letter to be transformed.
    Returns:
- char: The letter with its case reversed. If the input is lowercase, it returns the uppercase version,
and if the input is uppercase, it returns the lowercase version.
    >>> transform_letter_case('b')
    'Z'
*/
char transform_letter_case(char letter) {
    if (islower(letter)) {
        return toupper(letter);
    } else if (isupper(letter)) {
        return tolower(letter);
    }
    // If the input is not a letter, return it as-is
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
    return 0;
}