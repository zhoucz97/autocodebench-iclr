
/**
 * Determines if it is possible to assemble wooden squares from a number of buckets
 * into a single larger square. Each bucket contains a set number of squares,
 * with each square having a side length of 1.
 * 
 * @param length - The number of buckets.
 * @param squares - An array where each element represents the number of squares in a bucket.
 * @returns A boolean indicating whether it is possible to form a larger square.
 *
 * Examples:
 *   IsSquare(1, [9]) returns true.
 *   IsSquare(2, [14, 2]) returns true.
 *   IsSquare(2, [7, 7]) returns false.
 */

function isSquare(length: number, squares: number[]): boolean {
    const totalSquares = squares.reduce((sum, num) => sum + num, 0);
    const sqrt = Math.sqrt(totalSquares);
    return sqrt === Math.floor(sqrt);
}
const testIsSquare = (): void => {
    console.assert(isSquare(1, [9]) === true, "Test 1 failed");
    console.assert(isSquare(2, [14, 2]) === true, "Test 2 failed");
    console.assert(isSquare(2, [7, 7]) === false, "Test 3 failed");
    console.assert(isSquare(7, [1, 2, 3, 4, 5, 6, 7]) === false, "Test 4 failed");
    console.assert(isSquare(6, [1, 3, 5, 7, 9, 11]) === true, "Test 5 failed");
    console.assert(isSquare(4, [2, 2, 2, 2]) === false, "Test 6 failed");
    
    // Additional test cases
    console.assert(isSquare(3, [4, 5, 6]) === false, "Additional Test 1 failed");
    console.assert(isSquare(4, [16, 9, 4, 1]) === false, "Additional Test 2 failed");
    console.assert(isSquare(5, [1, 1, 1, 1, 1]) === false, "Additional Test 3 failed");
    console.assert(isSquare(2, [25, 25]) === false, "Additional Test 4 failed");
    console.assert(isSquare(3, [10, 10, 5]) === true, "Additional Test 5 failed");
    
    // console.info("All tests passed!");
};

testIsSquare();