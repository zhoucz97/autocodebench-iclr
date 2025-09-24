#include <cctype>
#include <cassert>
#include <cstring>
#include <iostream>
/*
Decode a series of numbers to reveal the pattern and understand the actual values 
each digit represents.

Equations provided for reference:
0000=4 8888=8 1234=1 5678=3 9012=2 1357=0 2468=4

Parameters:
- data_str: A constant character array (C-style string) representing a series of numbers. Length does not exceed 100.

Returns:
int: The result of each data string corresponding to the right-hand side of the equation.

Example usage:
assert(decode_numbers("0000") == 4);
*/
int decode_numbers(const char* data_str) {
    int total = 0;
    const int closed_areas[] = {1, 0, 0, 0, 1, 0, 1, 0, 2, 1}; // For digits 0-9
    
    while (*data_str != '\0') {
        if (isdigit(*data_str)) {
            int digit = *data_str - '0';
            total += closed_areas[digit];
        }
        data_str++;
    }
    
    return total;
}
int main() {
    // Assert basic provided test cases
    assert(decode_numbers("0000") == 4);
    assert(decode_numbers("8888") == 8);
    assert(decode_numbers("1234") == 1);
    assert(decode_numbers("5678") == 3);
    assert(decode_numbers("9012") == 2);
    assert(decode_numbers("1357") == 0);
    assert(decode_numbers("2468") == 4);

    // Assert additional test cases
    assert(decode_numbers("9999") == 4);
    assert(decode_numbers("1111") == 0);
    assert(decode_numbers("2222") == 0);
    assert(decode_numbers("3333") == 0);
    assert(decode_numbers("4444") == 4);
    assert(decode_numbers("5555") == 0);
    assert(decode_numbers("6666") == 4);
    assert(decode_numbers("7777") == 0);
    assert(decode_numbers("0001") == 3);
    assert(decode_numbers("2301") == 1);

    // Indicate that all tests passed
    // std::cout << "All tests passed successfully!" << std::endl;

    return 0;
}