
/**
 * Classify the integer x based on the following method:
 * If x is a single-digit number, x belongs to its own class.
 * Otherwise, sum the digits of x, get a new x, and iterate until the class is determined.
 *
 * @param x - The string representation of the integer to be classified.
 * @returns The class to which the integer x belongs.
 * 
 * @example
 * ```typescript
 * classifyInteger("24") // returns 6
 * ```
 */

function classifyInteger(x: string): number {
    let numStr = x;
    while (numStr.length > 1) {
        let sum = 0;
        for (const char of numStr) {
            sum += parseInt(char, 10);
        }
        numStr = sum.toString();
    }
    return parseInt(numStr, 10);
}
const testClassifyInteger = (): void => {
    console.assert(classifyInteger("24") === 6, "Test failed for input '24'");
    console.assert(classifyInteger("39") === 3, "Test failed for input '39'");
    console.assert(classifyInteger("123456789") === 9, "Test failed for input '123456789'");
    console.assert(classifyInteger("123456789012345678901234567890") === 9, "Test failed for input '123456789012345678901234567890'");
    console.assert(classifyInteger("12345") === 6, "Test failed for input '12345'");
    console.assert(classifyInteger("999999999") === 9, "Test failed for input '999999999'");
};

testClassifyInteger();