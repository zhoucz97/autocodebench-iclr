
/**
 * Find the maximum and minimum of three distinct integers.
 *
 * @param a - The first integer.
 * @param b - The second integer.
 * @param c - The third integer.
 * @returns A tuple containing the maximum and minimum values respectively.
 *
 * @example
 * const [max, min] = findMaxMin(1, 2, 3);
 * console.assert(max === 3 && min === 1);
 */

function findMaxMin(a: number, b: number, c: number): [number, number] {
    const max = Math.max(a, b, c);
    const min = Math.min(a, b, c);
    return [max, min];
}

// Example usage:
const [max, min] = findMaxMin(1, 2, 3);
console.assert(max === 3 && min === 1);
(() => {
    let max: number, min: number, result: [number, number];

    result = findMaxMin(1, 2, 3);
    [max, min] = result;
    console.assert(max === 3 && min === 1, 'Test failed for input (1, 2, 3)');

    // Additional tests
    result = findMaxMin(5, 3, 4);
    [max, min] = result;
    console.assert(max === 5 && min === 3, 'Test failed for input (5, 3, 4)');

    result = findMaxMin(10, -2, 7);
    [max, min] = result;
    console.assert(max === 10 && min === -2, 'Test failed for input (10, -2, 7)');

    result = findMaxMin(-1, -3, -2);
    [max, min] = result;
    console.assert(max === -1 && min === -3, 'Test failed for input (-1, -3, -2)');
})();