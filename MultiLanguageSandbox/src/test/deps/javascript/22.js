
/**
 * Given integers c and d, where a + b = c and a * b = d, find and return the
 * possible value of a (a <= b). If there are multiple groups, output the group
 * with the smallest a.
 * 
 * Parameters:
 *   - c (int): The sum of a and b.
 *   - d (int): The product of a and b.
 * 
 * Returns:
 *   A possible value of a or -1 if valid values do not exist.
 * 
 * Examples:
 *   findIntegers(7, 11) // returns -1
 *   findIntegers(5, 6)  // returns 2
 */

function findIntegers(c, d) {
    const discriminant = c * c - 4 * d;
    if (discriminant < 0) {
        return -1;
    }
    const sqrtDiscriminant = Math.sqrt(discriminant);
    if (!Number.isInteger(sqrtDiscriminant)) {
        return -1;
    }
    const a1 = (c - sqrtDiscriminant) / 2;
    const a2 = (c + sqrtDiscriminant) / 2;
    if (Number.isInteger(a1) && a1 <= (c - a1)) {
        return a1;
    } else if (Number.isInteger(a2) && a2 <= (c - a2)) {
        return a2;
    } else {
        return -1;
    }
}
const testFindIntegers = () => {
    console.assert(findIntegers(5, 6) === 2, "Expected 2");
    console.assert(findIntegers(6, 9) === 3, "Expected 3");
    console.assert(findIntegers(7, 12) === 3, "Expected 3");
    console.assert(findIntegers(7, 11) === -1, "Expected -1");
    console.assert(findIntegers(9, 8) === 1, "Expected 1");
    console.assert(findIntegers(10, 25) === 5, "Expected 5");
    console.assert(findIntegers(10000, 8765) === -1, "Expected -1");
    // console.log("All tests passed successfully.");
}

testFindIntegers();