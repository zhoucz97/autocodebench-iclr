
/**
 * Counts the number of different coloring methods for `n` squares with `m` colors,
 * considering the requirement that adjacent squares and the first/last squares
 * must have different colors.
 * 
 * Args:
 * - n (number): The number of squares.
 * - m (number): The number of colors.
 * 
 * Returns:
 * - number: The count of different coloring methods satisfying the specified conditions.
 *           The result is modulo 1000003.
 * 
 * Example calls:
 * ```
 * countColoringMethods(1, 1) // returns 1
 * ```
 */

function countColoringMethods(n: number, m: number): number {
    const MOD = 1000003;
    if (n === 1) {
        return m % MOD;
    }
    let result = 0;
    if (m === 1) {
        return 0; // since all squares must be different, but only one color available
    }
    // The formula is (m-1)^n + (-1)^n * (m-1), then mod MOD
    const term1 = modPow(m - 1, n, MOD);
    const term2 = modPow(-1, n, MOD) * (m - 1) % MOD;
    result = (term1 + term2) % MOD;
    if (result < 0) {
        result += MOD;
    }
    return result;
}

function modPow(base: number, exponent: number, mod: number): number {
    let result = 1;
    base = base % mod;
    while (exponent > 0) {
        if (exponent % 2 === 1) {
            result = (result * base) % mod;
        }
        base = (base * base) % mod;
        exponent = Math.floor(exponent / 2);
    }
    return result;
}
function runTests() {
    console.assert(countColoringMethods(1, 1) === 1);
    console.assert(countColoringMethods(2, 2) === 2);
    console.assert(countColoringMethods(3, 3) === 6);
    console.assert(countColoringMethods(4, 2) === 2);
    // The expected result for this case is not provided in the C code, but it is required to pass the test
    console.assert(countColoringMethods(1000, 10) === 566585); 

    // Additional test cases.
    console.assert(countColoringMethods(2, 3) === 6);
    console.assert(countColoringMethods(1000, 1000) === 67911);
    console.assert(countColoringMethods(999, 66) === 501817);
    console.assert(countColoringMethods(5, 3) === 30); // Example of an expected output

}

runTests();