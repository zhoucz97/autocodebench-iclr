#include <stdio.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
/*
Given an array of n distinct integers representing the heights of Kira's friends, 
find the number of ways to choose a triplet (a, b, c) such that the greatest common divisor (GCD) 
of the maximum and minimum values of the triplet is 1.
    >>> countTriplets([1, 5, 7])
    1
*/
// Function to compute GCD of two numbers using Euclidean algorithm
int gcd(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

int countTriplets(int heights[], int n) {
    int count = 0;
    // Iterate through all possible triplets
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            for (int k = j + 1; k < n; k++) {
                // Find the max and min in the triplet
                int max_val = heights[i];
                if (heights[j] > max_val) max_val = heights[j];
                if (heights[k] > max_val) max_val = heights[k];
                
                int min_val = heights[i];
                if (heights[j] < min_val) min_val = heights[j];
                if (heights[k] < min_val) min_val = heights[k];
                
                // Check if GCD of max and min is 1
                if (gcd(max_val, min_val) == 1) {
                    count++;
                }
            }
        }
    }
    return count;
}

// Example usage
int main() {
    int heights1[] = {1, 5, 7};
    assert(countTriplets(heights1, 3) == 1);

    int heights2[] = {1, 6, 2, 3};
    assert(countTriplets(heights2, 4) == 3);

    int heights3[] = {16, 4, 8, 2};
    assert(countTriplets(heights3, 4) == 0);

    int heights4[] = {10, 1, 6, 7, 9, 8, 4, 3, 5, 2};
    assert(countTriplets(heights4, 10) == 77);

    int heights5[] = {4, 5, 9, 11, 14};
    assert(countTriplets(heights5, 5) == 7);

    int heights6[] = {15, 10, 6, 9, 7, 3, 12, 8, 11, 1, 2};
    assert(countTriplets(heights6, 11) == 104);

    int heights7[] = {3, 7, 11, 13};
    assert(countTriplets(heights7, 4) == 4);

    int heights8[] = {5, 12, 13, 17, 19};
    assert(countTriplets(heights8, 5) == 10);

    int heights9[] = {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
    assert(countTriplets(heights9, 11) == 87);

    int heights10[] = {1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
    assert(countTriplets(heights10, 11) == 122);

    return 0;
}