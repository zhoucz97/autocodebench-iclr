
/**
 * Given a rectangle paper of size n x m placed on a plane Cartesian coordinate system with its bottom-left 
 * corner at (0,0) and top-right corner at (n,m), you need to calculate the expected number of operations
 * to cut the paper such that the remaining area is less than k.
 * In each operation, a line is randomly chosen that is parallel to the axes, passes through points with
 * integer coordinates, and cuts through (not just touching the edge) the paper. The bottom or right part
 * of the paper along this line is then discarded.
 * The answer should be modulo 10^9+7.
 *
 * For example:
 *    expectedCuts(3, 3, 4) // returns 3
 *    expectedCuts(5, 5, 12) // returns 2
 */

const MOD = 1e9 + 7;

function expectedCuts(n, m, k) {
    // Create a DP table initialized to 0
    let dp = new Array(n + 1).fill(0).map(() => new Array(m + 1).fill(0));
    
    for (let i = 1; i <= n; i++) {
        for (let j = 1; j <= m; j++) {
            if (i * j < k) {
                dp[i][j] = 0;
                continue;
            }
            
            let total = 0;
            let count = 0;
            
            // Vertical cuts: x can be from 1 to i-1
            for (let x = 1; x < i; x++) {
                total += dp[x][j];
                count++;
            }
            
            // Horizontal cuts: y can be from 1 to j-1
            for (let y = 1; y < j; y++) {
                total += dp[i][y];
                count++;
            }
            
            // The expected value is 1 (current cut) + average of all possible next states
            if (count > 0) {
                total = (total + count) % MOD; // Adding count is equivalent to adding 1 for each term (1 + average)
                // But wait, the correct formula is 1 + (sum of dp[x][j] and dp[i][y]) / (number of cuts)
                // So total is sum of (dp[x][j] for all x) + sum of (dp[i][y] for all y)
                // The number of cuts is (i-1) + (j-1)
                // So the average is total / (i-1 + j-1)
                // But since we're working mod 1e9+7, we need to multiply by the modular inverse of (i-1 + j-1)
                let inv = modInverse(i - 1 + j - 1, MOD);
                dp[i][j] = (1 + total * inv) % MOD;
            } else {
                // This case shouldn't happen since i*j >=k implies at least one cut is possible
                dp[i][j] = 0;
            }
        }
    }
    
    return dp[n][m];
}

// Function to compute modular inverse using Fermat's little theorem
function modInverse(a, mod) {
    return power(a, mod - 2, mod);
}

function power(a, b, mod) {
    let result = 1;
    a = a % mod;
    while (b > 0) {
        if (b % 2 === 1) {
            result = (result * a) % mod;
        }
        a = (a * a) % mod;
        b = Math.floor(b / 2);
    }
    return result;
}
function testExpectedCuts() {
    console.assert(expectedCuts(2, 4, 10) === 0, 'Test 1 failed');
    console.assert(expectedCuts(2, 4, 8) === 1, 'Test 2 failed');
    console.assert(expectedCuts(2, 4, 2) === 833333342, 'Test 3 failed');
    console.assert(expectedCuts(2, 4, 6) === 250000003, 'Test 4 failed');
    console.assert(expectedCuts(3, 3, 4) === 666666673, 'Test 5 failed');
    console.assert(expectedCuts(5, 5, 12) === 666666673, 'Test 6 failed');
    console.assert(expectedCuts(6, 7, 20) === 722222229, 'Test 7 failed');
    console.assert(expectedCuts(10, 10, 50) === 714285721, 'Test 9 failed');
    console.assert(expectedCuts(1, 10, 5) === 945634929, 'Test 10 failed');
    console.assert(expectedCuts(10, 1, 5) === 945634929, 'Test 11 failed');
}

testExpectedCuts();