
/**
 * Given three digits a, b, and c, where two of them are equal and the third is different,
 * this function finds and returns the value that occurs exactly once.
 * Examples:
 * extraNumber(0, 0, 1) // returns 1
 * extraNumber(4, 3, 4) // returns 3
 */

function extraNumber(a, b, c) {
    if (a === b) {
        return c;
    } else if (a === c) {
        return b;
    } else {
        return a;
    }
}
const testExtraNumber = () => {
    console.assert(extraNumber(2, 7, 2) === 7, 'Test with 2, 7, 2 failed');
    console.assert(extraNumber(3, 2, 2) === 3, 'Test with 3, 2, 2 failed');
    console.assert(extraNumber(5, 5, 1) === 1, 'Test with 5, 5, 1 failed');
    console.assert(extraNumber(500000000, 3, 500000000) === 3, 'Test with 500000000, 3, 500000000 failed');
    console.assert(extraNumber(500000000, 500000000, 3) === 3, 'Test with 500000000, 500000000, 3 failed');
};

testExtraNumber();