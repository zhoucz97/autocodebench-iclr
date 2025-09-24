#include <iostream>
#include <cassert>
#include <cstdio>
/**
 * Displays the ASCII information for a given character.
 * 
 * @param character The input character for which ASCII information is to be displayed.
 * @return The corresponding ASCII code.
 * 
 * Example:
 * ```cpp
 * int asciiCode = display_ascii_info('A');
 * std::cout << asciiCode; // Output: 65
 * ```
 */
int display_ascii_info(char character) {
    int asciiCode = static_cast<int>(character);
    std::cout << "Character: " << character << std::endl;
    std::cout << "ASCII Code: " << asciiCode << std::endl;
    return asciiCode;
}
int main() {
    assert(display_ascii_info('A') == 65);
    assert(display_ascii_info('B') == 66); // Additional test case
    assert(display_ascii_info('0') == 48); // Additional test case
    assert(display_ascii_info(' ') == 32); // Additional test case
    
    // Uncomment the line below to print a success message if compiled outside a test framework
    // std::printf("All tests passed.\n");

    return 0;
}