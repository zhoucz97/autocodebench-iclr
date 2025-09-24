
/**
 * Reads an integer and a character, then returns them as a formatted string separated by a comma.
 * @param integer_value The input integer.
 * @param char_value The input character.
 * @returns A string containing the integer and character separated by a comma.
 * @example
 * processIntegerAndChar(234, 'H')  // Returns "234,H"
 */

function processIntegerAndChar(integer_value: number, char_value: string): string {
    return `${integer_value},${char_value}`;
}
const testProcessIntegerAndChar = (): void => {
    console.assert(processIntegerAndChar(234, 'H') === "234,H", "Test 1 Failed");
    console.assert(processIntegerAndChar(123, 'A') === "123,A", "Test 2 Failed");
    console.assert(processIntegerAndChar(0, 'Z') === "0,Z", "Test 3 Failed");
    
    // Additional tests can be added here with different input values
    // Example:
    console.assert(processIntegerAndChar(567, 'X') === "567,X", "Example Test Failed");
};

testProcessIntegerAndChar();