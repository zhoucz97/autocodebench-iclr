#include <iostream>
#include <unordered_set>
#include <cmath>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
/**
 * Given two positive integers n and k, find the number of positive integers x,
 * where 1 <= x <= n, that can be expressed as x = a^b with a and b being
 * positive integers and b >= k. Different legal representations of x are counted
 * only once. For example:
 *     >>> countPowerNumbers(99, 1)
 *     99
 */
using namespace std;

int countPowerNumbers(int n, int k) {
    unordered_set<int> powerNumbers;
    
    for (int a = 2; ; ) {
        long long current = 1;
        bool overflow = false;
        for (int b = k; ; b++) {
            current *= a;
            if (current > n) {
                overflow = true;
                break;
            }
            powerNumbers.insert(static_cast<int>(current));
        }
        if (overflow) break;
        a++;
        if (a > pow(n, 1.0 / k) + 1) break; // To prevent infinite loop, though mathematically a^k >n would stop earlier
    }
    
    return powerNumbers.size();
}

// For testing purposes
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