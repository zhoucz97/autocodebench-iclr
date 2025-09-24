#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
/*
Given a rectangle paper of size n x m placed on a plane Cartesian coordinate system with its bottom-left corner at (0,0) and top-right corner at (n,m), 
you need to calculate the expected number of operations to cut the paper such that the remaining area is less than k. 
In each operation, a line is randomly chosen that is parallel to the axes, passes through points with integer coordinates, and cuts through (not just touching the edge) the paper. The bottom or right part of the paper along this line is then discarded.
The answer should be modulo 10^9+7.

For example:
    >>> expectedCuts(3, 3, 4)
    3
*/
#define MOD 1000000007

long long dp[1001][1001];
long long inv[2001]; // Precompute inverses up to 2000 (since max n or m is 1000, sum is 2000)

void precomputeInverses() {
    for (int i = 1; i <= 2000; ++i) {
        inv[i] = 1;
        long long power = i;
        long long mod = MOD - 2;
        while (mod > 0) {
            if (mod % 2 == 1) {
                inv[i] = (inv[i] * power) % MOD;
            }
            power = (power * power) % MOD;
            mod /= 2;
        }
    }
}

long long expectedCuts(int n, int m, long long k) {
    memset(dp, 0, sizeof(dp));
    precomputeInverses();
    
    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= m; ++j) {
            if ((long long)i * j < k) {
                dp[i][j] = 0;
                continue;
            }
            long long total = 0;
            long long cnt = 0;
            
            // Vertical cuts (x = 1..i-1)
            for (int x = 1; x < i; ++x) {
                cnt++;
                total = (total + dp[x][j]) % MOD;
            }
            
            // Horizontal cuts (y = 1..j-1)
            for (int y = 1; y < j; ++y) {
                cnt++;
                total = (total + dp[i][y]) % MOD;
            }
            
            if (cnt == 0) {
                dp[i][j] = 0;
            } else {
                long long inv_cnt = inv[cnt];
                long long avg = (total * inv_cnt) % MOD;
                dp[i][j] = (avg + 1) % MOD;
            }
        }
    }
    
    return dp[n][m];
}

// Example usage (not part of the solution)
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