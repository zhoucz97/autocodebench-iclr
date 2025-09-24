
#include <assert.h>
#include <stdio.h>
/*
Given an array A of integers, the task is to calculate the sum of the XOR of all subarrays.
A subarray is defined by a pair of indices (L, R) such that 1 <= L <= R <= n, where n is the size of the array.
The XOR sum of a subarray is the result of XORing all elements from L to R.
The final result is the sum of the XOR sums for all possible subarrays.

Example cases:
    >>> sumOfXorSubarrays({1, 2, 3, 4, 5}, 5)
    39
*/
long long sumOfXorSubarrays(int A[], int n) {
    long long total = 0;
    for (int i = 0; i < n; ++i) {
        long long current_xor = 0;
        for (int j = i; j < n; ++j) {
            current_xor ^= A[j];
            total += current_xor;
        }
    }
    return total;
}
int main() {
    int test[] = {1,2, 3, 4, 5};
    assert(sumOfXorSubarrays(test, 5) == 39);
    int test1[] = {1, 1, 1};
    assert(sumOfXorSubarrays(test1, 3) == 4);
    
    int test2[] = {2, 3, 1};
    assert(sumOfXorSubarrays(test2, 3) == 9);
    
    int test3[] = {4, 5, 7, 9};
    assert(sumOfXorSubarrays(test3, 4) == 74);
    
    int test4[] = {0, 0, 0, 0};
    assert(sumOfXorSubarrays(test4, 4) == 0);
    
    int test5[] = {8, 8, 8, 8, 8};
    assert(sumOfXorSubarrays(test5, 5) == 72);
    
    int test6[] = {3, 6, 9, 12, 15};
    assert(sumOfXorSubarrays(test6, 5) == 125);
    
    int test7[] = {10, 20, 30, 40, 50};
    assert(sumOfXorSubarrays(test7, 5) == 390);
    
    int test8[] = {16, 16, 16, 16, 16, 16};
    assert(sumOfXorSubarrays(test8, 6) == 192);
    
    int test9[] = {1, 3, 5, 7, 9, 11, 13};
    assert(sumOfXorSubarrays(test9, 7) == 192);
    
    int test10[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    assert(sumOfXorSubarrays(test10, 10) == 218);
    
    return 0;
}