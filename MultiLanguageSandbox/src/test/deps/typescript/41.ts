
/**
 * Computes the number of sequences consisting of n '(' and m ')', such that the longest balanced
 * subsequence is of length 2 * k, modulo 1,000,000,007 (10^9 + 7).
 * 
 * Example:
 *   countBalancedSubsequences(2, 2, 2) returns 2
 *
 * @param n The number of '(' characters.
 * @param m The number of ')' characters.
 * @param k The half-length of the longest balanced subsequence.
 * @returns The number of valid sequences modulo 10^9 + 7.
 */

const MOD = 1e9 + 7;

function comb(n: number, k: number, memo: Map<string, number>): number {
    if (k < 0 || k > n) return 0;
    if (k === 0 || k === n) return 1;
    const key = `${n},${k}`;
    if (memo.has(key)) return memo.get(key)!;
    
    const res = (comb(n - 1, k - 1, memo) + comb(n - 1, k, memo)) % MOD;
    memo.set(key, res);
    return res;
}

function countBalancedSubsequences(n: number, m: number, k: number): number {
    if (Math.min(n, m) !== k) {
        return 0;
    }
    
    // The sequences must have exactly k pairs, and the rest are unbalanced.
    // The total sequences is C(n + m, n), but we need to subtract those where min(n', m') > k in any subsequence.
    // However, the problem reduces to counting sequences where the entire sequence has exactly k pairs, and no longer balanced subsequence exists.
    // This is equivalent to sequences where the number of '(' and ')' are exactly k each, and the rest are unbalanced.
    // But the example shows that for n=2, m=2, k=2, the answer is 2, which are the two sequences that are fully balanced.
    // So the general case is when n == m == k, the answer is the number of fully balanced sequences of length 2k, which is the Catalan number C_k.
    // The Catalan number C_k is (1/(k+1)) * C(2k, k).
    
    if (n !== m || n !== k) {
        return 0;
    }
    
    // Compute the Catalan number C_k = (1/(k+1)) * C(2k, k) mod MOD.
    let catalan = 1;
    for (let i = 1; i <= k; i++) {
        catalan = catalan * (n + i) / i % MOD;
    }
    catalan = catalan * inv(k + 1, MOD) % MOD;
    return catalan;
}

// Function to compute modular inverse using Fermat's little theorem since MOD is prime.
function inv(a: number, mod: number): number {
    return pow(a, mod - 2, mod);
}

function pow(a: number, b: number, mod: number): number {
    let result = 1;
    a = a % mod;
    while (b > 0) {
        if (b % 2 === 1) {
            result = result * a % mod;
        }
        a = a * a % mod;
        b = Math.floor(b / 2);
    }
    return result;
}
(() => {
    console.assert(countBalancedSubsequences(2, 2, 2) === 2, 'Test case 1 failed');
    console.assert(countBalancedSubsequences(3, 2, 3) === 0, 'Test case 2 failed');
    console.assert(countBalancedSubsequences(3, 2, 1) === 4, 'Test case 3 failed');
    console.assert(countBalancedSubsequences(4, 3, 2) === 14, 'Test case 4 failed');
    console.assert(countBalancedSubsequences(5, 5, 2) === 35, 'Test case 5 failed');
    console.assert(countBalancedSubsequences(6, 1, 1) === 6, 'Test case 6 failed');
    console.assert(countBalancedSubsequences(1, 6, 1) === 6, 'Test case 7 failed');
    console.assert(countBalancedSubsequences(7, 2, 2) === 27, 'Test case 8 failed');
    console.assert(countBalancedSubsequences(8, 3, 3) === 110, 'Test case 9 failed');
    console.assert(countBalancedSubsequences(10, 10, 5) === 10659, 'Test case 10 failed');
    console.assert(countBalancedSubsequences(20, 20, 10) === 574221648, 'Test case 11 failed');
    console.assert(countBalancedSubsequences(2000, 2000, 1000) === 854104531, 'Test case 12 failed');
    console.assert(countBalancedSubsequences(2000, 1999, 1000) === 334874485, 'Test case 13 failed');
    console.assert(countBalancedSubsequences(2000, 2000, 1999) === 259428024, 'Test case 14 failed');
})();