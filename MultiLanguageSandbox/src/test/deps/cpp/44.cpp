#include <vector>
#include <algorithm>
#include <numeric>
#include <cassert>
#include <algorithm>
#include <vector>
/**
 * Given an array of n distinct integers representing the heights of Kira's friends, 
 * find the number of ways to choose a triplet (a, b, c) such that the greatest common divisor (GCD) 
 * of the maximum and minimum values of the triplet is 1.
 * 
 * Example usage:
 * >>> countTriplets({1, 5, 7})
 * 1
 */
// for gcd

using namespace std;

int countTriplets(const vector<int>& heights) {
    int n = heights.size();
    int count = 0;
    
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j < n; ++j) {
            for (int k = j + 1; k < n; ++k) {
                int a = heights[i];
                int b = heights[j];
                int c = heights[k];
                
                int max_val = max({a, b, c});
                int min_val = min({a, b, c});
                
                if (gcd(max_val, min_val) == 1) {
                    count++;
                }
            }
        }
    }
    
    return count;
}
int main() {
    assert(countTriplets({1, 5, 7}) == 1);
    assert(countTriplets({1, 6, 2, 3}) == 3);
    assert(countTriplets({16, 4, 8, 2}) == 0);
    assert(countTriplets({10, 1, 6, 7, 9, 8, 4, 3, 5, 2}) == 77);
    assert(countTriplets({4, 5, 9, 11, 14}) == 7);
    assert(countTriplets({15, 10, 6, 9, 7, 3, 12, 8, 11, 1, 2}) == 104);
    assert(countTriplets({3, 7, 11, 13}) == 4);
    assert(countTriplets({5, 12, 13, 17, 19}) == 10);
    assert(countTriplets({2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}) == 87);
    assert(countTriplets({1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17}) == 122);

    return 0;
}