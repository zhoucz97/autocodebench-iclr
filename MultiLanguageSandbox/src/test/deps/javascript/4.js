
/**
 * Find the maximum and minimum of three distinct integers.
 *
 * Parameters:
 * a (number): The first integer.
 * b (number): The second integer.
 * c (number): The third integer.
 *
 * Returns:
 * Object: An object with properties 'max' and 'min'.
 *
 * Example call:
 * const result = findMaxMin(1, 2, 3);
 * console.assert(result.max === 3 && result.min === 1);
 */

function findMaxMin(a, b, c) {
    // Find the maximum value
    const max = Math.max(a, b, c);
    
    // Find the minimum value
    const min = Math.min(a, b, c);
    
    // Return an object with max and min properties
    return { max, min };
}

// Example call
const result = findMaxMin(1, 2, 3);
console.assert(result.max === 3 && result.min === 1); // This should pass without any errors
function testFindMaxMin() {
    let result = findMaxMin(1, 2, 3);
    console.assert(result.max === 3 && result.min === 1);

    // Additional tests
    result = findMaxMin(5, 3, 4);
    console.assert(result.max === 5 && result.min === 3);

    result = findMaxMin(10, -2, 7);
    console.assert(result.max === 10 && result.min === -2);

    result = findMaxMin(-1, -3, -2);
    console.assert(result.max === -1 && result.min === -3);
}

testFindMaxMin();