
/**
 * Given a non-negative integer num, repeatedly add all its digits until
 * the result has only one digit. 
 * For example:
 *   addDigits(38)  // returns 2
 *   Explanation: 3 + 8 = 11, and 1 + 1 = 2. Since 2 has only one digit, 2 is the result.
 */

function addDigits(num: number): number {
    while (num >= 10) {
        let sum = 0;
        while (num > 0) {
            sum += num % 10;
            num = Math.floor(num / 10);
        }
        num = sum;
    }
    return num;
}
const testAddDigits = (): void => {
    console.assert(addDigits(38) === 2, 'Test 1 failed');
    console.assert(addDigits(0) === 0, 'Test 2 failed');
    console.assert(addDigits(9) === 9, 'Test 3 failed');
    console.assert(addDigits(123) === 6, 'Test 4 failed');
    console.assert(addDigits(456) === 6, 'Test 5 failed');
    console.assert(addDigits(9999) === 9, 'Test 6 failed');
    console.assert(addDigits(100) === 1, 'Test 7 failed');
    console.assert(addDigits(1010) === 2, 'Test 8 failed');
    console.assert(addDigits(1234) === 1, 'Test 9 failed');
    console.assert(addDigits(9876) === 3, 'Test 10 failed');
    console.assert(addDigits(199) === 1, 'Test 11 failed');
};

testAddDigits();