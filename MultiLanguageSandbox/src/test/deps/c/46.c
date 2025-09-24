
#include <assert.h>
#include <stdio.h>
/*
Given an integer n, write a function to compute the sum of all numbers from 1 to n that are multiples of either 3 or 5. If a number is a multiple of both 3 and 5, it should only be counted once.
For example:
    >>> sumOfMultiples(10)
    33 // (3 + 5 + 6 + 9 + 10)
*/
int sumOfMultiples(int n) {
    int sum = 0;
    for (int i = 1; i <= n; i++) {
        if (i % 3 == 0 || i % 5 == 0) {
            sum += i;
        }
    }
    return sum;
}
int main()
{
    assert(sumOfMultiples(10) == 33);
    assert(sumOfMultiples(15) == 60);
    assert(sumOfMultiples(20) == 98);
    assert(sumOfMultiples(5) == 8);
    assert(sumOfMultiples(3) == 3);
    assert(sumOfMultiples(6) == 14);
    assert(sumOfMultiples(9) == 23);
    assert(sumOfMultiples(12) == 45);
    assert(sumOfMultiples(17) == 60);
    assert(sumOfMultiples(21) == 119);
    assert(sumOfMultiples(25) == 168);
    return 0;
}