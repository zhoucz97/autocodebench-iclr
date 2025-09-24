
/**
 * Given a permutation q of n elements and an integer k, find the number of permutations p of n elements
 * such that f(p) = q, where f(p) is the lexicographically smallest permutation that can be obtained by
 * dividing p into exactly k non-empty contiguous segments and sorting each segment (i.e., choose k-1
 * breakpoints 1 <= x1 < x2 < ... < x(k-1) < n, and divide it into [1, x1], (x1, x2], ..., (x(k-1), n]).
 * The result should be modulo 998244353.
 * 
 * @example
 * countPermutations(2, 1, [1, 2])
 * // returns 2
 */

const MOD = 998244353;

const countPermutations = (n, k, q) => {
    // Check if q can be split into k increasing segments
    let segments = 1;
    for (let i = 1; i < n; i++) {
        if (q[i] < q[i - 1]) {
            segments++;
        }
    }
    if (segments !== k) {
        return 0;
    }
    
    // Now, compute the product of factorials of the segment lengths
    let result = 1;
    let currentSegmentLength = 1;
    for (let i = 1; i < n; i++) {
        if (q[i] > q[i - 1]) {
            currentSegmentLength++;
        } else {
            result = (result * factorial(currentSegmentLength)) % MOD;
            currentSegmentLength = 1;
        }
    }
    result = (result * factorial(currentSegmentLength)) % MOD;
    
    return result;
};

// Precompute factorials up to n (since n can be up to a certain limit, but here we handle dynamically)
function factorial(m) {
    if (m === 0 || m === 1) return 1;
    let res = 1;
    for (let i = 2; i <= m; i++) {
        res = (res * i) % MOD;
    }
    return res;
}
console.assert(countPermutations(2, 1, [1, 2]) === 2, 'Test failed for input ([1, 2])');
console.assert(countPermutations(3, 3, [3, 1, 2]) === 1, 'Test failed for input ([3, 1, 2])');
console.assert(countPermutations(6, 3, [1, 2, 3, 6, 5, 4]) === 13, 'Test failed for input ([1, 2, 3, 6, 5, 4])');
console.assert(countPermutations(6, 1, [1, 2, 3, 4, 5, 6]) === 720, 'Test failed for input ([1, 2, 3, 4, 5, 6])');
console.assert(countPermutations(6, 3, [1, 2, 5, 3, 4, 5]) === 0, 'Test failed for input ([1, 2, 5, 3, 4, 5])');
console.assert(countPermutations(9, 9, [1, 2, 3, 4, 5, 6, 7, 8, 9]) === 1, 'Test failed for input ([1, 2, 3, 4, 5, 6, 7, 8, 9])');
console.assert(countPermutations(9, 2, [1, 2, 3, 4, 5, 6, 7, 9, 8]) === 29093);