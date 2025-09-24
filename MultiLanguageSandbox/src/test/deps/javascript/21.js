
/**
 * Determines if it is possible to assemble the wooden squares from n buckets,
 * where each bucket contains a_i squares with a side length of 1, into a single larger square.
 * Input: length of the list, array of numbers
 * @param {number} length - The number of buckets.
 * @param {number[]} squares - An array of numbers, where each number represents the count of squares in a bucket.
 * @returns {boolean} - Returns true if it's possible to form a perfect square, otherwise returns false.
 *
 * Examples:
 * Is_Square(1, [9]) // true
 */

function Is_Square(length, squares) {
    const totalSquares = squares.reduce((sum, num) => sum + num, 0);
    const sqrt = Math.sqrt(totalSquares);
    return sqrt === Math.floor(sqrt);
}
const testIsSquare = () => {
    console.assert(Is_Square(1, [9]) === true);
    console.assert(Is_Square(2, [14, 2]) === true);
    console.assert(Is_Square(2, [7, 7]) === false);
    console.assert(Is_Square(7, [1, 2, 3, 4, 5, 6, 7]) === false);
    console.assert(Is_Square(6, [1, 3, 5, 7, 9, 11]) === true);
    console.assert(Is_Square(4, [2, 2, 2, 2]) === false);

    // Additional test cases
    console.assert(Is_Square(3, [4, 5, 6]) === false);
    console.assert(Is_Square(4, [16, 9, 4, 1]) === false);
    console.assert(Is_Square(5, [1, 1, 1, 1, 1]) === false);
    console.assert(Is_Square(2, [25, 25]) === false);
    console.assert(Is_Square(3, [10, 10, 5]) === true);

    // console.log("All tests passed!");
}

testIsSquare();