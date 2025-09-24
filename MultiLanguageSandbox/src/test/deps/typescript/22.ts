
/**
 * Given integers c and d, where a + b = c and a * b = d, find and return the
 * possible value of a (a <= b). If there are multiple pairs, output the pair with
 * the smallest a.
 * 
 * @param c - The sum of a and b.
 * @param d - The product of a and b.
 * @returns A possible value of a or -1 if valid values do not exist.
 *
 * Examples:
 *   findIntegers(7, 11) // -1
 *   findIntegers(5, 6)  // 2
 */

function findIntegers(c: number, d: number): number {
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
    
    if (Number.isInteger(a1) && Number.isInteger(a2)) {
        return Math.min(a1, a2);
    } else {
        return -1;
    }
}
const testFindIntegers = () => {
    console.assert(findIntegers(5, 6) === 2, 'Test 1 failed');
    console.assert(findIntegers(6, 9) === 3, 'Test 2 failed');
    console.assert(findIntegers(7, 12) === 3, 'Test 3 failed');
    console.assert(findIntegers(7, 11) === -1, 'Test 4 failed');
    console.assert(findIntegers(9, 8) === 1, 'Test 5 failed');
    console.assert(findIntegers(10, 25) === 5, 'Test 6 failed');
    console.assert(findIntegers(10000, 8765) === -1, 'Test 7 failed');

    // console.log("All tests passed successfully.");
};

testFindIntegers();