
/**
 * Given two integers a and b, return the sum if the sum is even,
 * or return the product of a and b if the sum is odd.
 * Examples:
 *   evenSumOrOddProduct(2, 3) returns 6
 */

function evenSumOrOddProduct(a, b) {
    const sum = a + b;
    if (sum % 2 === 0) {
        return sum;
    } else {
        return a * b;
    }
}
const testEvenSumOrOddProduct = () => {
    console.assert(evenSumOrOddProduct(2, 3) === 6, 'Test Case 1 Failed');
    console.assert(evenSumOrOddProduct(5, 5) === 10, 'Test Case 2 Failed');
    console.assert(evenSumOrOddProduct(1, 1) === 2, 'Test Case 3 Failed');
    console.assert(evenSumOrOddProduct(0, 0) === 0, 'Test Case 4 Failed');
    console.assert(evenSumOrOddProduct(-1, -1) === -2, 'Test Case 5 Failed');
    console.assert(evenSumOrOddProduct(100, 200) === 300, 'Test Case 6 Failed');
    console.assert(evenSumOrOddProduct(3, 4) === 12, 'Test Case 7 Failed');
    console.assert(evenSumOrOddProduct(-5, 5) === 0, 'Test Case 8 Failed');
    console.assert(evenSumOrOddProduct(7, 8) === 56, 'Test Case 9 Failed');
    console.assert(evenSumOrOddProduct(9, 10) === 90, 'Test Case 10 Failed');
    console.assert(evenSumOrOddProduct(11, 14) === 154, 'Test Case 11 Failed');
}

testEvenSumOrOddProduct();