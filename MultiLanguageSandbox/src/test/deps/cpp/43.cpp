#include <vector>
#include <algorithm>
#include <bits/stdc++.h>
/*
Given a permutation q of n elements and an integer k, find the number of permutations p of n elements such that f(p) = q, where f(p) is the lexicographically smallest permutation that can be obtained by dividing p into exactly k non-empty contiguous segments and sorting each segment (i.e., choose k-1 breakpoints 1 <= x_1 < x_2 < ... < x_{k-1} < n, and divide it into [1, x_1], (x_1, x_2], ..., (x_{k-1}, n]). The result should be modulo 998244353.

    >>> countPermutations(2, 1, [1, 2])
    2

*/
using namespace std;

const int MOD = 998244353;

// Precompute factorials up to n (assuming n is up to some reasonable limit, say 2e5)
vector<int> fact;

void precomputeFactorials(int max_n) {
    fact.resize(max_n + 1);
    fact[0] = 1;
    for (int i = 1; i <= max_n; ++i) {
        fact[i] = 1LL * fact[i-1] * i % MOD;
    }
}

int countPermutations(int n, int k, int qq[]) {
    // Check if the permutation q has exactly k increasing runs
    vector<int> q(qq, qq + n);
    int run_count = 1;
    for (int i = 1; i < n; ++i) {
        if (q[i] < q[i-1]) {
            run_count++;
        }
    }
    
    if (run_count != k) {
        return 0;
    }
    
    // Now, compute the product of the factorials of the lengths of each run
    // First, find the positions where runs start (i.e., where q[i] < q[i-1])
    vector<int> run_starts;
    run_starts.push_back(0);
    for (int i = 1; i < n; ++i) {
        if (q[i] < q[i-1]) {
            run_starts.push_back(i);
        }
    }
    
    // The lengths of the runs are the differences between consecutive run starts
    long long result = 1;
    for (int i = 0; i < run_starts.size(); ++i) {
        int start = run_starts[i];
        int end = (i == run_starts.size() - 1) ? n : run_starts[i+1];
        int length = end - start;
        result = result * fact[length] % MOD;
    }
    
    return static_cast<int>(result);
}

// Example usage:
//
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