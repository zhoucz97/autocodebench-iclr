
/**
 * Count the number of different permutation schemes for a binary string of length n,
 * where the number of '1's is m and the number of '0's is n - m.
 *
 * @param {number} n - Length of the binary string.
 * @param {number} m - Number of '1's in the binary string.
 *
 * @returns {number} The total number of different permutation schemes.
 */

function countPermutationsOfBinaryString(n, m) {
    if (m < 0 || m > n) {
        return 0;
    }
    // Calculate the binomial coefficient C(n, m)
    let result = 1;
    // To optimize, we use the multiplicative formula: C(n, m) = product from k=1 to m of (n - m + k) / k
    for (let k = 1; k <= m; k++) {
        result = result * (n - m + k) / k;
    }
    return Math.round(result); // Rounding to handle potential floating point inaccuracies
}
function testCountPermutationsOfBinaryString() {
    console.assert(countPermutationsOfBinaryString(2, 0) === 2);
    console.assert(countPermutationsOfBinaryString(2, 1) === 0);
    console.assert(countPermutationsOfBinaryString(3, 0) === 0);
    console.assert(countPermutationsOfBinaryString(3, 1) === 3);
    console.assert(countPermutationsOfBinaryString(3, 2) === 0);
    console.assert(countPermutationsOfBinaryString(30, 2) === 145422675);
    console.assert(countPermutationsOfBinaryString(4, 2) === 4);
    console.assert(countPermutationsOfBinaryString(5, 5) === 1);
    console.assert(countPermutationsOfBinaryString(33, 17) === 13884156);
    console.assert(countPermutationsOfBinaryString(1000, 1000) === 1);
    // Add more test cases if necessary
}

testCountPermutationsOfBinaryString();