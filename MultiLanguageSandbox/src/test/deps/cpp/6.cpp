#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
/**
 * Calculates the factorial of N modulo 10007.
 * 
 * Parameters:
 * - N (int): An integer representing the input value (N <= 10000).
 * 
 * Returns:
 * int: The result after calculating the factorial of N and
 * taking the modulo 10007.
 * 
 * Examples:
 * >>> process_request(1)
 * 1
 * >>> process_request(10)
 * 6266
 */
int process_request(int n) {
    if (n < 0) return 0; // Factorial is not defined for negative numbers
    if (n == 0 || n == 1) return 1; // 0 and 1 are both 1

    int result = 1;
    for (int i = 2; i <= n; ++i) {
        result = (result * i) % 10007; // Multiply and take modulo at each step to prevent overflow
    }
    return result;
}

// Example usage:
int main()
{
    assert(process_request(0) == 1); // Added test for boundary condition
    assert(process_request(1) == 1);
    assert(process_request(2) == 2);
    assert(process_request(3) == 6);
    assert(process_request(4) == 24);
    assert(process_request(10) == 6266); // 10! % 10007 = 3628800 % 10007 = 6266
    assert(process_request(10000) == 6991); // Added a test for upper boundary condition
    // printf("All tests passed.\n");
    return 0;
}