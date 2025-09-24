#include <math.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Given integers c and d, where a + b = c and a * b = d, find and return the possible value of a (a <= b). If there are multiple groups, output the group with the smallest a.
    Parameters:
    - c (int): The sum of a and b.
    - d (int): The product of a and b.

    Returns:
    Optional[int]: A possible value of a. Returns -1 if valid values do not exist,
\
    >>> find_integers(7,11)
    -1
*/
int find_integers(int c, int d) {
    // Calculate the discriminant
    long long D = (long long)c * c - 4 * d;
    
    if (D < 0) {
        return -1; // No real roots
    }
    
    long long sqrt_D = (long long)sqrt(D);
    if (sqrt_D * sqrt_D != D) {
        return -1; // Discriminant is not a perfect square
    }
    
    // Check if (c - sqrt_D) is even to ensure integer roots
    if ((c - sqrt_D) % 2 != 0) {
        return -1;
    }
    
    int a = (c - sqrt_D) / 2;
    int b = (c + sqrt_D) / 2;
    
    // Ensure a <= b
    if (a > b) {
        // This shouldn't happen if calculations are correct, but just in case
        return -1;
    }
    
    return a;
}
int main() {
    assert(find_integers(5, 6) == 2);
    assert(find_integers(6, 9) == 3);
    assert(find_integers(7, 12) == 3);
    assert(find_integers(7, 11) == -1);
    assert(find_integers(9, 8) == 1);
    assert(find_integers(10, 25) == 5);
    assert(find_integers(10000, 8765) == -1);

    

    return 0;
}