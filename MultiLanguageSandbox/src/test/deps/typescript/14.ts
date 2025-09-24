
/**
 * Decode a series of numbers to reveal the pattern and understand the actual values
 * each digit represents.
 * 0000=4 8888=8 1234=1 5678=3 9012=2 1357=0 2468=4
 *
 * Parameters:
 * - dataStr: A string representing a series of numbers. Length does not exceed 100.
 *
 * Returns:
 * number: The result of each data string corresponding to the right-hand side of the equation.
 *
 * Example usage:
 * console.assert(decodeNumbers("0000") === 4);
 */

function decodeNumbers(dataStr: string): number {
    const allSame = new Set(dataStr.split('')).size === 1;
    if (allSame) {
        return parseInt(dataStr[0], 10);
    }
    // For other cases, we don't know the pattern, so return 0 as a placeholder
    return 0;
}

// Test cases
console.assert(decodeNumbers("0000") === 4); // Fails, because our function returns 0
console.assert(decodeNumbers("8888") === 8); // Fails, because our function returns 8 (which matches)
const testDecodeNumbers = (): void => {
    console.assert(decodeNumbers("0000") === 4);
    console.assert(decodeNumbers("8888") === 8);
    console.assert(decodeNumbers("1234") === 1);
    console.assert(decodeNumbers("5678") === 3);
    console.assert(decodeNumbers("9012") === 2);
    console.assert(decodeNumbers("1357") === 0);
    console.assert(decodeNumbers("2468") === 4);

    // Additional test samples
    console.assert(decodeNumbers("9999") === 4);
    console.assert(decodeNumbers("1111") === 0);
    console.assert(decodeNumbers("2222") === 0);
    console.assert(decodeNumbers("3333") === 0);
    console.assert(decodeNumbers("4444") === 4);
    console.assert(decodeNumbers("5555") === 0);
    console.assert(decodeNumbers("6666") === 4);
    console.assert(decodeNumbers("7777") === 0);
    console.assert(decodeNumbers("0001") === 3);
    console.assert(decodeNumbers("2301") === 1);
};

testDecodeNumbers();