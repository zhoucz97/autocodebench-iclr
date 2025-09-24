#include <vector>
#include <unordered_map>
#include <cassert>
#include <iostream>
#include <vector>
/**
 * Given an array A of integers, the task is to calculate the sum of the XOR of all subarrays.
 * A subarray is defined by a pair of indices (L, R) such that 1 <= L <= R <= n, where n is the size of the array.
 * The XOR sum of a subarray is the result of XORing all elements from L to R.
 * The final result is the sum of the XOR sums for all possible subarrays.
 *
 * Example cases:
 *     >>> sumOfXorSubarrays({1, 2, 3, 4, 5}, 5)
 *     39
 */
using namespace std;

long long sumOfXorSubarrays(const vector<int>& A) {
    long long total = 0;
    int n = A.size();
    
    for (int i = 0; i < n; ++i) {
        int current_xor = 0;
        for (int j = i; j < n; ++j) {
            current_xor ^= A[j];
            total += current_xor;
        }
    }
    
    return total;
}
int main() {
    assert(sumOfXorSubarrays({1, 2, 3, 4, 5}) == 39);
    assert(sumOfXorSubarrays({1, 1, 1}) == 4);
    assert(sumOfXorSubarrays({2, 3, 1}) == 9);
    assert(sumOfXorSubarrays({4, 5, 7, 9}) == 74);
    assert(sumOfXorSubarrays({0, 0, 0, 0}) == 0);
    assert(sumOfXorSubarrays({8, 8, 8, 8, 8}) == 72);
    assert(sumOfXorSubarrays({3, 6, 9, 12, 15}) == 125);
    assert(sumOfXorSubarrays({10, 20, 30, 40, 50}) == 390);
    assert(sumOfXorSubarrays({16, 16, 16, 16, 16, 16}) == 192);
    assert(sumOfXorSubarrays({1, 3, 5, 7, 9, 11, 13}) == 192);
    assert(sumOfXorSubarrays({0, 1, 2, 3, 4, 5, 6, 7, 8, 9}) == 218);


    return 0;
}