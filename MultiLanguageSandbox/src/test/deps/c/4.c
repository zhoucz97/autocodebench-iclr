
#include <assert.h>
#include <stdio.h>
/*
Find the maximum and minimum of three distinct integers.
Parameters:
a (int): The first integer.
b (int): The second integer.
c (int): The third integer.

Returns:
Tuple[int, int]: A tuple is not a concept in C; instead, we will return by reference.

>>> Example call: 
int max, min;
find_max_min(1,2,3, &max, &min);
assert(max == 3 && min == 1);
*/
void find_max_min(int a, int b, int c, int *max, int *min) {
    // Find the maximum
    if (a > b) {
        if (a > c) {
            *max = a;
        } else {
            *max = c;
        }
    } else {
        if (b > c) {
            *max = b;
        } else {
            *max = c;
        }
    }
    
    // Find the minimum
    if (a < b) {
        if (a < c) {
            *min = a;
        } else {
            *min = c;
        }
    } else {
        if (b < c) {
            *min = b;
        } else {
            *min = c;
        }
    }
}
int main() {
    int max, min;

    find_max_min(1, 2, 3, &max, &min);
    assert(max == 3 && min == 1);

    // Additional tests
    find_max_min(5, 3, 4, &max, &min);
    assert(max == 5 && min == 3);

    find_max_min(10, -2, 7, &max, &min);
    assert(max == 10 && min == -2);

    find_max_min(-1, -3, -2, &max, &min);
    assert(max == -1 && min == -3);

    return 0;
}