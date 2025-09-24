#include <climits>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
/**
 * Find the length of the longest consecutive sequence of 1s in the binary 
 * representation of a non-negative integer.
 *
 * Parameters:
 * - n (unsigned long long): A non-negative integer (0 ≤ n ≤ 2^64 - 1).
 *
 * Returns:
 * - int: The length of the longest consecutive sequence of 1s in the binary 
 *        representation of the given integer.
 *
 * Examples:
 *   >>> find_longest_consecutive_ones_length(7)
 *   3
 */
int find_longest_consecutive_ones_length(unsigned long long n) {
    int max_length = 0;
    int current_length = 0;
    
    while (n != 0) {
        if (n & 1) {  // Check if the least significant bit is 1
            current_length++;
            if (current_length > max_length) {
                max_length = current_length;
            }
        } else {
            current_length = 0;
        }
        n >>= 1;  // Right shift to check the next bit
    }
    
    return max_length;
}
int main()
{
    assert(find_longest_consecutive_ones_length(7) == 3);
    assert(find_longest_consecutive_ones_length(13) == 2);
    assert(find_longest_consecutive_ones_length(12345) == 3); // New test sample
    assert(find_longest_consecutive_ones_length(0b11011101111) == 4); // New test sample using binary literal for clarity
    assert(find_longest_consecutive_ones_length(0xFFFFFFFF) == 32); // New test sample: all ones for a 32-bit number
    assert(find_longest_consecutive_ones_length(0) == 0); // New test sample: no ones in a zero

    // printf("All tests passed!\n"); // In C++, you might use std::cout, but it is commented out as per the original code.
    return 0;
}