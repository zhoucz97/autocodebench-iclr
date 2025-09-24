
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Find the factorial of N and take the modulo 10007 of the result.
​    Parameters:
- N (int): An integer representing the input value (N <= 10000).
​    Returns:
​    int: The result after taking the modulo 10007 of the output.
    >>> process_request(1)
    1
*/
int process_request(int n) {
    if (n < 0) {
        return 0; // Factorial is not defined for negative numbers
    }
    
    int result = 1;
    for (int i = 1; i <= n; i++) {
        result = (result * i) % 10007; // Take modulo at each step to prevent overflow
    }
    
    return result;
}
int main()
{
    assert(process_request(0) == 1); // Added test for boundary condition
    assert(process_request(1) == 1);
    assert(process_request(2) == 2);
    assert(process_request(3) == 6);
    assert(process_request(4) == 24);
    assert(process_request(10) == 6266); // 10! % 10007 = 3628800 % 10007 = 362
    assert(process_request(10000) == 6991); // Added a test for upper boundary condition
    // printf("All tests passed.\n");
    return 0;
}