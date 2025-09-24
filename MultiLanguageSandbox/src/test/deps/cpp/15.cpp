#include <iostream>
#include <cassert>
#include <iostream>
/**
 * Counts the number of different coloring methods for n squares with m colors,
 * considering the requirement that adjacent squares and the first/last squares
 * must have different colors.
 * 
 * @param n The number of squares.
 * @param m The number of colors.
 * @return The count of different coloring methods satisfying the specified conditions.
 *         The result is modulo 1000003.
 *
 * Examples:
 * count_coloring_methods(1,1) returns 1
 * count_coloring_methods(2,2) returns 2
 */
using namespace std;

const int MOD = 1000003;

long long power(long long base, long long exp) {
    long long result = 1;
    base = base % MOD;
    while (exp > 0) {
        if (exp % 2 == 1) {
            result = (result * base) % MOD;
        }
        base = (base * base) % MOD;
        exp = exp / 2;
    }
    return result;
}

int count_coloring_methods(int n, int m) {
    if (n == 1) {
        return 1;
    }
    if (m == 1) {
        return 0; // because adjacent squares (including first and last) must be different, which is impossible with one color for n > 1
    }
    long long term1 = power(m - 1, n);
    long long term2 = (n % 2 == 0) ? (m - 1) : (MOD - (m - 1));
    long long total = (term1 + term2) % MOD;
    return static_cast<int>(total);
}

// Test cases
int main() {
    assert(count_coloring_methods(1, 1) == 1);
    assert(count_coloring_methods(2, 2) == 2);
    assert(count_coloring_methods(3, 3) == 6);
    assert(count_coloring_methods(4, 2) == 2);
    assert(count_coloring_methods(1000, 10) == 566585); // We don't have the expected result for this case

    // Additional test cases
    assert(count_coloring_methods(2, 3) == 6);
    assert(count_coloring_methods(1000, 1000) == 67911);
    assert(count_coloring_methods(999, 66) == 501817);
    assert(count_coloring_methods(5, 3) == 30); // Example of an expected output

    // std::cout << "All tests passed!\n";
    return 0;
}