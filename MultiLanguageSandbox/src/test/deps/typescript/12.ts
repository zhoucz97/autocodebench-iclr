
/**
 * You are given three digits a, b, c. Two of them are equal, but the third one
 * is different from the other two. Find the value that occurs exactly once.
 * 
 * Examples:
 * ```
 * extraNumber(0, 0, 1) // returns 1
 * ```
 */

function extraNumber(a: number, b: number, c: number): number {
    if (a === b) {
        return c;
    } else if (a === c) {
        return b;
    } else {
        return a;
    }
}
(() => {
    console.assert(extraNumber(2, 7, 2) === 7, "Test 1 failed");
    console.assert(extraNumber(3, 2, 2) === 3, "Test 2 failed");
    console.assert(extraNumber(5, 5, 1) === 1, "Test 3 failed");
    console.assert(extraNumber(500000000, 3, 500000000) === 3, "Test 4 failed");
    console.assert(extraNumber(500000000, 500000000, 3) === 3, "Test 5 failed");

    // console.log("All tests passed!");
})();