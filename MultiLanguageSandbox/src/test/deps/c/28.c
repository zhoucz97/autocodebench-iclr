#include <stdio.h>
#include <stdio.h>
#include <assert.h>
/*
Display the ASCII information for a given character.
​    Parameters:
- character (char): The input character for which ASCII information is to be displayed.
​    Returns:
- int: corresponding ASCII code
    >>> display_ascii_info('A')
    65
*/

int display_ascii_info(char character) {
    // Display the ASCII information
    printf("Character: %c\n", character);
    printf("ASCII Code: %d\n", (int)character);
    
    // Return the ASCII code
    return (int)character;
}
int main() {
    assert(display_ascii_info('A') == 65);
    assert(display_ascii_info('B') == 66); // Additional test case
    assert(display_ascii_info('0') == 48); // Additional test case
    assert(display_ascii_info(' ') == 32); // Additional test case
    // printf("All tests passed.\n");
    return 0;
}