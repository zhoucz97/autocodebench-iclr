#include <vector>
#include <algorithm>
#include <cassert>
#include <cstdio>
/**
 * Given a rectangle paper of size n x m placed on a plane Cartesian coordinate 
 * system with its bottom-left corner at (0,0) and top-right corner at (n,m), 
 * you need to calculate the expected number of operations to cut the paper such that 
 * the remaining area is less than k. In each operation, a line is randomly chosen 
 * that is parallel to the axes, passes through points with integer coordinates, 
 * and cuts through (not just touching the edge) the paper. The bottom or right part 
 * of the paper along this line is then discarded. The answer should be modulo 10^9+7.
 *
 * For example:
 *     >>> expectedCuts(3, 3, 4)
 *     3
 *     >>> expectedCuts(5, 5, 12)
 *     2
 */
using namespace std;

const int MOD = 1e9 + 7;
const int inv2 = 500000004; // Modular inverse of 2 under MOD

long long expectedCuts(int n, int m, long long k) {
    vector<vector<long long>> dp(n + 1, vector<long long>(m + 1, 0));
    
    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= m; ++j) {
            if (1LL * i * j < k) {
                dp[i][j] = 0;
                continue;
            }
            long long total = 0;
            // Horizontal cuts
            for (int x = 1; x < i; ++x) {
                long long left = dp[x][j];
                long long right = dp[i - x][j];
                total += (left + right + 2) / 2; // Each cut contributes 1, and the two parts' expectations
            }
            // Vertical cuts
            for (int y = 1; y < j; ++y) {
                long long top = dp[i][y];
                long long bottom = dp[i][j - y];
                total += (top + bottom + 2) / 2;
            }
            // Total possible cuts: (i-1) horizontal + (j-1) vertical
            int total_cuts = (i - 1) + (j - 1);
            if (total_cuts == 0) {
                dp[i][j] = 0;
            } else {
                dp[i][j] = (total * inv2) % MOD;
            }
        }
    }
    return dp[n][m];
}
int main() {
    assert(expectedCuts(2, 4, 10) == 0);
    assert(expectedCuts(2, 4, 8) == 1);
    assert(expectedCuts(2, 4, 2) == 833333342);
    assert(expectedCuts(2, 4, 6) == 250000003);
    assert(expectedCuts(3, 3, 4) == 666666673);
    assert(expectedCuts(5, 5, 12) == 666666673);
    assert(expectedCuts(6, 7, 20) == 722222229);
    assert(expectedCuts(8, 8, 30) == 72727275);
    assert(expectedCuts(10, 10, 50) == 714285721);
    assert(expectedCuts(1, 10, 5) == 945634929);
    assert(expectedCuts(10, 1, 5) == 945634929);
    return 0;
}