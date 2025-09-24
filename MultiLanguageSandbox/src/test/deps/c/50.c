
#include <assert.h>
#include <stdio.h>
/*
Given two integers a and b, return the sum if the sum is even, or return the product of a and b if the sum is odd.
    >>> evenSumOrOddProduct(2, 3)
    6
    >>> evenSumOrOddProduct(5, 5)
    10
*/
int evenSumOrOddProduct(int a, int b) {
    int sum = a + b;
    if (sum % 2 == 0) {
        return sum;
    } else {
        return a * b;
    }
}
int main() {
    assert(evenSumOrOddProduct(2, 3) == 6);
    assert(evenSumOrOddProduct(5, 5) == 10);
    assert(evenSumOrOddProduct(1, 1) == 2);
    assert(evenSumOrOddProduct(0, 0) == 0);
    assert(evenSumOrOddProduct(-1, -1) == -2);
    assert(evenSumOrOddProduct(100, 200) == 300);
    assert(evenSumOrOddProduct(3, 4) == 12);
    assert(evenSumOrOddProduct(-5, 5) == 0);
    assert(evenSumOrOddProduct(7, 8) == 56);
    assert(evenSumOrOddProduct(9, 10) == 90);
    assert(evenSumOrOddProduct(11, 14) == 154);
    return 0;
}