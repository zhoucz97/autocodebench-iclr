#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
/*
Given a permutation q of n elements and an integer k, find the number of permutations p of n elements such that f(p) = q, where f(p) is the lexicographically smallest permutation that can be obtained by dividing p into exactly k non-empty contiguous segments and sorting each segment(that is, choose $ k-1 $ breakpoints $ 1\le x_1 < x_2 < \cdots < x_{k-1} < n $, and divide it into $ [1, x_1], (x_1, x_2], \cdots, (x_{k-1}, n] $). The result should be modulo 998244353.
    >>> countPermutations(2, 1, [1, 2])
    2

*/
#define MOD 998244353

// Precompute factorial up to n (assuming n is up to some reasonable limit, say 2000)
long long fact[2001];

void precomputeFactorials(int max_n) {
    fact[0] = 1;
    for (int i = 1; i <= max_n; i++) {
        fact[i] = (fact[i-1] * i) % MOD;
    }
}

int countPermutations(int n, int k, int qq[]) {
    // Check if q is composed of k non-decreasing segments
    int segments = 1;
    for (int i = 1; i < n; i++) {
        if (qq[i] < qq[i-1]) {
            segments++;
        }
    }
    if (segments != k) {
        return 0;
    }
    
    // Now, compute the product of factorials of segment lengths
    long long result = 1;
    int current_segment_length = 1;
    for (int i = 1; i < n; i++) {
        if (qq[i] >= qq[i-1]) {
            current_segment_length++;
        } else {
            result = (result * fact[current_segment_length]) % MOD;
            current_segment_length = 1;
        }
    }
    result = (result * fact[current_segment_length]) % MOD;
    
    return (int)result;
}

// Example usage (not part of the solution)
int main() {
    int q1[] = {1, 2};
    assert(countPermutations(2, 1, q1) == 2);

    int q2[] = {3, 1, 2};
    assert(countPermutations(3, 3, q2) == 1);

    int q3[] = {1, 2, 3, 6, 5, 4};
    assert(countPermutations(6, 3, q3) == 13);

    int q4[] = {1, 2, 3, 4, 5, 6};
    assert(countPermutations(6, 1, q4) == 720);

    int q5[] = {1, 2, 5, 3, 4, 5};
    assert(countPermutations(6, 3, q5) == 0);

    int q6[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    assert(countPermutations(9, 9, q6) == 1);

    int q7[] = {1, 2, 3, 4, 5, 6, 7, 9, 8};
    assert(countPermutations(9, 2, q7) == 29093);
    return 0;}