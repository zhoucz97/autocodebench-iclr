#include <iostream>
#include <cassert>
#include <cstdio>
/**
 * This problem introduces a custom modulo operation, denoted by "⊕".
 * When calculating x ⊕ y, if x is not a multiple of y, the result is x % y.
 * If x is a multiple of y, divide x by y repeatedly until x is no longer a
 * multiple of y, denoting this final value as x'. Then the result is x' % y.
 * For example:
 * - 4⊕5=4,
 * - 20⊕5=4,
 * - 100⊕5=4.
 * Given a prime number p, you need to handle several queries where you're given
 * an integer n and you must compute n! ⊕ p. Here, n! is the factorial of n.
 * Example:
 *    newModuloFactorial(3, 7) returns 6
 */
unsigned long long newModuloFactorial(unsigned long long n, unsigned long long p) {
    if (n == 0 || n == 1) {
        return 1 % p;
    }
    
    unsigned long long factorial = 1;
    for (unsigned long long i = 2; i <= n; ++i) {
        factorial *= i;
    }
    
    // Apply the custom modulo operation
    while (factorial % p == 0 && factorial != 0) {
        factorial /= p;
    }
    
    return factorial % p;
}

// Example usage
int main() {
    // Test cases
    assert(newModuloFactorial(3, 7) == 6);
    assert(newModuloFactorial(10, 3) == 1);
    assert(newModuloFactorial(11, 7) == 4);
    assert(newModuloFactorial(45, 7) == 1);
    assert(newModuloFactorial(14, 7) == 2);
    assert(newModuloFactorial(1919, 10007) == 3152);
    assert(newModuloFactorial(810, 10007) == 3679);
    assert(newModuloFactorial(3, 7) == 6);
    assert(newModuloFactorial(10, 3) == 1);
    assert(newModuloFactorial(1, 2) == 1);
    assert(newModuloFactorial(5, 11) == 10);
    assert(newModuloFactorial(6, 13) == 5);
    assert(newModuloFactorial(8, 17) == 13);
    assert(newModuloFactorial(15, 19) == 16);
    assert(newModuloFactorial(21, 23) == 1);
    assert(newModuloFactorial(30, 29) == 28);
    assert(newModuloFactorial(100, 101) == 100);
    return 0;
}