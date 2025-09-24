#include <stdio.h>
#include <assert.h>
#include <stdio.h>
/*
Counts the number of different coloring methods for n squares with m colors,
considering the requirement that adjacent squares and the first/last squares
must have different colors.
    Args:
- n (int): The number of squares.
- m (int): The number of colors.
    Returns:
    int: The count of different coloring methods satisfying the specified conditions. Result is modulo 1000003.
    >>> count_coloring_methods(1,1)
    1
*/
#define MOD 1000003

long long pow_mod(long long base, long long exp) {
    long long result = 1;
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
        return m % MOD;
    }
    long long total_linear = (m * pow_mod(m - 1, n - 1)) % MOD;
    long long same_first_last = ((m - 1) * pow_mod(m - 2, n - 2)) % MOD;
    long long result = (total_linear - same_first_last + MOD) % MOD; // Ensure non-negative
    return (int)result;
}
int main() {
    assert(count_coloring_methods(1, 1) == 1);
    assert(count_coloring_methods(2, 2) == 2);
    assert(count_coloring_methods(3, 3) == 6);
    assert(count_coloring_methods(4, 2) == 2);
    assert(count_coloring_methods(1000, 10) == 566585); // We don't have the expected result for this case

    // Additional test cases
    assert(count_coloring_methods(2, 3) == 6);
    assert(count_coloring_methods(1000, 1000) == 67911);
    assert(count_coloring_methods(999,66) == 501817);
    assert(count_coloring_methods(5, 3) == 30); // Example of an expected output

    // printf("All tests passed!\n");
    return 0;
}