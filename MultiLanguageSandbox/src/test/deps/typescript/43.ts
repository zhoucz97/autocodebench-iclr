
/**
 * Given a permutation q of n elements and an integer k, find the number of permutations p of n elements
 * such that f(p) = q, where f(p) is the lexicographically smallest permutation that can be obtained by
 * dividing p into exactly k non-empty contiguous segments and sorting each segment (i.e., choose k-1
 * breakpoints 1 <= x1 < x2 < ... < x(k-1) < n, and divide it into [1, x1], (x1, x2], ..., (x(k-1), n]).
 * The result should be modulo 998244353.
 *
 * @example
 * countPermutations(2, 1, [1, 2]) // returns 2
 */

const MOD = 998244353;

function countPermutations(n: number, k: number, q: number[]): number {
    // Precompute factorials up to n modulo MOD
    const fact = new Array(n + 1).fill(1);
    for (let i = 1; i <= n; i++) {
        fact[i] = (fact[i - 1] * i) % MOD;
    }

    // Find all the positions where q[i] > q[i+1] (potential split points)
    const splitPoints = [];
    for (let i = 0; i < n - 1; i++) {
        if (q[i] > q[i + 1]) {
            splitPoints.push(i + 1); // 1-based position after q[i]
        }
    }

    const m = splitPoints.length;
    if (k - 1 > m) {
        return 0; // Not enough split points to create k segments
    }

    // We need to choose (k-1) split points from the available m split points
    // The number of ways is C(m, k-1)
    // Then, for each choice, the segments' lengths are determined, and the product of their factorials is multiplied.
    // However, since all choices lead to the same product (because the segments' lengths are fixed by the split points),
    // we can compute it once.

    // The segments are between 0 and splitPoints[0], splitPoints[0] and splitPoints[1], ..., splitPoints[m-1] and n.
    // The number of segments is m + 1. We need exactly k segments, so m + 1 == k => m == k - 1.
    // So, the only possible case is when the number of split points is exactly k-1.
    if (m !== k - 1) {
        return 0;
    }

    // The segments are defined by the split points. The lengths of the segments are:
    let res = 1;
    let prev = 0;
    for (let i = 0; i < k - 1; i++) {
        const len = splitPoints[i] - prev;
        res = (res * fact[len]) % MOD;
        prev = splitPoints[i];
    }
    // Last segment
    const len = n - prev;
    res = (res * fact[len]) % MOD;

    return res;
}

// Example usage:
 // Output: 2
console.assert(countPermutations(2, 1, [1, 2]) === 2, 'Test failed for input ([1, 2])');
console.assert(countPermutations(3, 3, [3, 1, 2]) === 1, 'Test failed for input ([3, 1, 2])');
console.assert(countPermutations(6, 3, [1, 2, 3, 6, 5, 4]) === 13, 'Test failed for input ([1, 2, 3, 6, 5, 4])');
console.assert(countPermutations(6, 1, [1, 2, 3, 4, 5, 6]) === 720, 'Test failed for input ([1, 2, 3, 4, 5, 6])');
console.assert(countPermutations(6, 3, [1, 2, 5, 3, 4, 5]) === 0, 'Test failed for input ([1, 2, 5, 3, 4, 5])');
console.assert(countPermutations(9, 9, [1, 2, 3, 4, 5, 6, 7, 8, 9]) === 1, 'Test failed for input ([1, 2, 3, 4, 5, 6, 7, 8, 9])');
console.assert(countPermutations(9, 2, [1, 2, 3, 4, 5, 6, 7, 9, 8]) === 29093);