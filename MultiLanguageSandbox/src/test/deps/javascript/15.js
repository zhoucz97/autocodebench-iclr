// No need to import modules in JavaScript like in C
/**
 * Counts the number of different coloring methods for n squares with m colors,
 * considering the requirement that adjacent squares and the first/last squares
 * must have different colors.
 * @param {number} n - The number of squares.
 * @param {number} m - The number of colors.
 * @return {number} The count of different coloring methods satisfying the conditions,
 *                  result is modulo 1000003.
 * Example:
 * count_coloring_methods(1,1) // 1
 * count_coloring_methods(2,3) // 6
 */

function count_coloring_methods(n, m) {
    const MOD = 1000003;
    if (n === 1) {
        return m % MOD;
    }
    let same = 0; // same[i] represents the number of ways where the first and i-th squares are the same
    let diff = m * (m - 1) % MOD; // diff[i] represents the number of ways where the first and i-th squares are different
    for (let i = 3; i <= n; i++) {
        const newSame = diff;
        const newDiff = (same + diff) * (m - 2) % MOD;
        same = newSame;
        diff = newDiff;
    }
    return diff;
}
(function testCountColoringMethods() {
    console.assert(count_coloring_methods(1, 1) === 1, 'Test case 1 failed');
    console.assert(count_coloring_methods(2, 2) === 2, 'Test case 2 failed');
    console.assert(count_coloring_methods(3, 3) === 6, 'Test case 3 failed');
    console.assert(count_coloring_methods(4, 2) === 2, 'Test case 4 failed');
    // For large numbers like 1000, 10 in JavaScript sometimes it might not be possible 
    // to directly assert the expected result since it is not provided in the original code.
    console.assert(count_coloring_methods(2, 3) === 6, 'Test case 5 failed');
    console.assert(count_coloring_methods(1000, 1000) === 67911, 'Test case 6 failed');
    console.assert(count_coloring_methods(999, 66) === 501817, 'Test case 7 failed');
    console.assert(count_coloring_methods(5, 3) === 30, 'Test case 8 failed');
    
    // console.log("All tests passed!");
})();