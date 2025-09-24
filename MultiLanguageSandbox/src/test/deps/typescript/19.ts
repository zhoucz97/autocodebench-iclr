
/*
Count the number of different permutation schemes for a binary string of length n,
where the number of '1's is m and the number of '0's is n - m.

Parameters:
- n (number): Length of the binary string.
- m (number): Number of '1's in the binary string.

Returns:
number: The total number of different permutation schemes.
*/

function countPermutationsOfBinaryString(n: number, m: number): number {
    if (m < 0 || m > n) {
        return 0;
    }
    // Calculate the binomial coefficient C(n, m)
    let result = 1;
    // To minimize the number of multiplications, we use the smaller of m and n-m
    const k = Math.min(m, n - m);
    for (let i = 1; i <= k; i++) {
        result = result * (n - k + i) / i;
    }
    return result;
}
(() => {
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
})();