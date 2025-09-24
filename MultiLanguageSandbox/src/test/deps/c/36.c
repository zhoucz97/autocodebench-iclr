
#include <assert.h>
#include <stdio.h>
/*
Given n doors arranged in a circle, the player starts in front of door 1. Each turn, the player can choose a number i and pay a cost C_i to move i steps to the right and then open the door at that position. It is guaranteed that C_i >= C_{i+1} for 1 <= i < n.
The task is to determine the minimum total cost required to open all doors.

Example:
    >>> minTotalCost(3, (1, 1, 1))
    3
*/
long long minTotalCost(int n, int *C) {
    long long total = 0;
    for (int i = 0; i < n; i++) {
        total += C[i];
    }
    return total;
}
int main() {
    int costs1[] = {4, 3, 3, 3, 3}; // 1-indexed array
    assert(minTotalCost(5, costs1) == 15);

    int costs2[] = {1, 1, 1};
    assert(minTotalCost(3, costs2) == 3);

    int costs3[] = {5, 4, 3, 2};
    assert(minTotalCost(4, costs3) == 11);

    int costs4[] = {100, 99, 98, 97};
    assert(minTotalCost(4, costs4) == 391);

    int costs5[] = {10, 9, 8, 7, 6, 5};
    assert(minTotalCost(6, costs5) == 35);

    int costs6[] = {2, 2, 2, 2, 2, 2, 2};
    assert(minTotalCost(7, costs6) == 14);

    int costs7[] = {9, 7, 7, 7, 7, 7, 7, 7};
    assert(minTotalCost(8, costs7) == 56);

    int costs8[] = {3, 2, 2, 2, 2, 2, 2, 2, 2};
    assert(minTotalCost(9, costs8) == 18);

    int costs9[] = {6, 5, 5, 5, 5, 5, 5, 5, 5, 5};
    assert(minTotalCost(10, costs9) == 50);

    int costs10[] = {8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
    assert(minTotalCost(11, costs10) == 11);

    return 0;
}