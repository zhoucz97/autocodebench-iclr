
#include <cassert>
#include <cmath>
#include <cstdio>
/**
 * You are given three integers a, b, c, where two of them are equal, and the
 * third is different from the other two. Your task is to find the value that
 * occurs exactly once.
 *
 * Examples:
 *   extraNumber(0, 0, 1) returns 1
 *   extraNumber(4, 3, 4) returns 3
 */
int extraNumber(int a, int b, int c) {
    if (a == b) {
        return c;
    } else if (a == c) {
        return b;
    } else {
        return a;
    }
}
int main() {
    assert(extraNumber(2, 7, 2) == 7);
    assert(extraNumber(3, 2, 2) == 3);
    assert(extraNumber(5, 5, 1) == 1);
    assert(extraNumber(500000000, 3, 500000000) == 3);
    assert(extraNumber(500000000, 500000000, 3) == 3);

    
    return 0;
}