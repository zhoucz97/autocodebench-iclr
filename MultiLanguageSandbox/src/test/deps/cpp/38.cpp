#include <vector>
#include <algorithm>
#include <cassert>
#include <cstdio>
/**
 * Given n positive integers representing the count of each number from 1 to n,
 * find the maximum sum of the mode (most frequent element) for all prefixes of
 * a sequence constructed from these numbers. The mode is the largest number among
 * the most frequent elements in a sequence.
 * 
 * Example:
 * >>> maxModeSum(3, {1, 2, 3})
 * 17
 * A sequence that reaches its maximum value is (3,2,3,1,2,2).
 */
using namespace std;

long long maxModeSum(int n, const int* counts) {
    vector<pair<int, int>> numCounts;
    for (int i = 0; i < n; ++i) {
        numCounts.emplace_back(n - i, counts[i]); // numbers are 1..n, stored in descending order
    }
    
    vector<int> sequence;
    for (const auto& [num, cnt] : numCounts) {
        for (int i = 0; i < cnt; ++i) {
            sequence.push_back(num);
        }
    }
    
    long long sum = 0;
    vector<int> freq(n + 1, 0); // 1-based indexing
    
    for (int i = 0; i < sequence.size(); ++i) {
        int num = sequence[i];
        freq[num]++;
        
        int maxFreq = 0;
        int mode = 0;
        for (int j = n; j >= 1; --j) {
            if (freq[j] > maxFreq) {
                maxFreq = freq[j];
                mode = j;
            }
        }
        sum += mode;
    }
    
    return sum;
}
int main() {
    int a1[] = {1, 3, 2};
    int a2[] = {4, 1, 2, 3};
    int a3[] = {1, 1};
    int a4[] = {1, 2, 3, 4, 5};
    int a5[] = {100000};
    int a6[] = {5, 3, 2, 4, 1};
    int a7[] = {100000, 100000, 100000};
    int a8[] = {2, 2, 5};
    int a9[] = {4, 4, 4, 4};
    int a10[] = {1, 2, 3, 4, 5, 6};
    int a11[] = {3, 1, 2};

    assert(maxModeSum(3, a1) == 17);
    assert(maxModeSum(4, a2) == 37);
    assert(maxModeSum(2, a3) == 4);
    assert(maxModeSum(5, a4) == 75);
    assert(maxModeSum(1, a5) == 100000);
    assert(maxModeSum(5, a6) == 62);
    assert(maxModeSum(3, a7) == 900000);
    assert(maxModeSum(3, a8) == 27);
    assert(maxModeSum(4, a9) == 64);
    assert(maxModeSum(6, a10) == 126);
    assert(maxModeSum(3, a11) == 16);
    return 0;
}