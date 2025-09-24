#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
/*
Given two positive integers n and k, find the number of positive integers x, where 1 <= x <= n, that can be expressed as x = a^b with a and b being positive integers and b >= k.
Different legal representations of the same number are counted only once.
    >>> countPowerNumbers(99, 1)
    99
*/
int countPowerNumbers(int n, int k) {
    bool *isPower = (bool *)calloc(n + 1, sizeof(bool));
    int count = 0;
    
    for (int a = 1; ; a++) {
        long long power = 1;
        bool overflow = false;
        for (int b = k; ; b++) {
            power *= a;
            if (power > n) {
                overflow = true;
                break;
            }
            if (!isPower[(int)power]) {
                isPower[(int)power] = true;
                count++;
            }
        }
        if (overflow) {
            break;
        }
    }
    
    free(isPower);
    return count;
}
int main() {
    assert(countPowerNumbers(99, 1) == 99);
    assert(countPowerNumbers(99, 3) == 7);
    assert(countPowerNumbers(99, 2) == 12);
    assert(countPowerNumbers(10, 1) == 10);
    assert(countPowerNumbers(10, 2) == 4);
    assert(countPowerNumbers(500, 1) == 500);
    assert(countPowerNumbers(500, 2) == 30);
    assert(countPowerNumbers(500, 3) == 13);
    assert(countPowerNumbers(1000, 1) == 1000);
    assert(countPowerNumbers(1000, 2) == 41);
    assert(countPowerNumbers(1000, 3) == 17);
    assert(countPowerNumbers(1000, 93) == 1);
    assert(countPowerNumbers(50, 2) == 10);
    assert(countPowerNumbers(50, 3) == 5);
    assert(countPowerNumbers(2, 3) == 1);
    return 0;
}