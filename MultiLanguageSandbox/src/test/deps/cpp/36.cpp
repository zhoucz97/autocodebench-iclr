#include <vector>
#include <algorithm>
#include <climits>
#include <cassert>
#include <iostream>
#include <vector>
/**
 * @doc
 * Given n doors arranged in a circle, the player starts in front of door 1.
 * Each turn, the player can choose a number i and pay a cost C_i to move i steps to the right
 * and then open the door at that position. It is guaranteed that C_i >= C_{i+1} for 1 <= i < n.
 * The task is to determine the minimum total cost required to open all doors.
 *
 * Example:
 *     >>> minTotalCost(3, (1, 1, 1))
 *     3
 */
using namespace std;

long long minTotalCost(int n, const vector<int>& C) {
    vector<long long> dp(n + 1, LLONG_MAX);
    dp[0] = 0; // base case: 0 cost to open 0 doors
    
    for (int k = 1; k <= n; ++k) {
        for (int i = 1; i <= k; ++i) {
            if (dp[k - i] != LLONG_MAX) {
                dp[k] = min(dp[k], dp[k - i] + C[i - 1]);
            }
        }
    }
    
    return dp[n];
}
int main() {
    std::vector<int> costs1 = {4, 3, 3, 3, 3}; // 1-indexed array
    assert(minTotalCost(5, costs1) == 15);

    std::vector<int> costs2 = {1, 1, 1};
    assert(minTotalCost(3, costs2) == 3);

    std::vector<int> costs3 = {5, 4, 3, 2};
    assert(minTotalCost(4, costs3) == 11);

    std::vector<int> costs4 = {100, 99, 98, 97};
    assert(minTotalCost(4, costs4) == 391);

    std::vector<int> costs5 = {10, 9, 8, 7, 6, 5};
    assert(minTotalCost(6, costs5) == 35);

    std::vector<int> costs6 = {2, 2, 2, 2, 2, 2, 2};
    assert(minTotalCost(7, costs6) == 14);

    std::vector<int> costs7 = {9, 7, 7, 7, 7, 7, 7, 7};
    assert(minTotalCost(8, costs7) == 56);

    std::vector<int> costs8 = {3, 2, 2, 2, 2, 2, 2, 2, 2};
    assert(minTotalCost(9, costs8) == 18);

    std::vector<int> costs9 = {6, 5, 5, 5, 5, 5, 5, 5, 5, 5};
    assert(minTotalCost(10, costs9) == 50);

    std::vector<int> costs10 = {8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
    assert(minTotalCost(11, costs10) == 11);


    return 0;
}