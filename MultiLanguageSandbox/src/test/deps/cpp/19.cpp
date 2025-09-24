#include <iostream>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
/**
 * Count the number of different permutation schemes for a binary string of length n,
 * where the number of '1's is m and the number of '0's is n - m.
 * 
 * Parameters:
 * - n (int): Length of the binary string.
 * - m (int): Number of '1's in the binary string.
 * 
 * Returns:
 * - int: The total number of different permutation schemes.
 */
using namespace std;

// Function to compute the binomial coefficient C(n, m)
long long binomialCoefficient(int n, int m) {
    if (m > n - m) {
        m = n - m; // Take advantage of symmetry C(n, k) = C(n, n-k)
    }
    long long result = 1;
    for (int i = 0; i < m; ++i) {
        result *= (n - i);
        result /= (i + 1);
    }
    return result;
}

int count_permutations_of_binary_string(int n, int m) {
    if (m < 0 || m > n) {
        return 0; // Invalid input, no such binary string exists
    }
    return binomialCoefficient(n, m);
}
int main() {
    assert(count_permutations_of_binary_string(2, 0) == 2);
    assert(count_permutations_of_binary_string(2, 1) == 0);
    assert(count_permutations_of_binary_string(3, 0) == 0);
    assert(count_permutations_of_binary_string(3, 1) == 3);
    assert(count_permutations_of_binary_string(3, 2) == 0);
    assert(count_permutations_of_binary_string(30, 2) == 145422675);
    assert(count_permutations_of_binary_string(4, 2) == 4);
    assert(count_permutations_of_binary_string(5, 5) == 1);
    assert(count_permutations_of_binary_string(33, 17) == 13884156);
    assert(count_permutations_of_binary_string(1000, 1000) == 1);
    // Add more test cases if necessary
    return 0;
}