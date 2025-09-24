
/**
 * Reads an integer and a character, then returns them as a formatted string separated by a comma.
 * Parameters:
 * - integer_value (number): The input integer.
 * - char_value (string): The input character.
 * Returns:
 * - string: A string containing the integer and character separated by a comma.
 * Examples:
 * process_integer_and_char(234, 'H') returns '234,H'
 */

function process_integer_and_char(integer_value, char_value) {
    return `${integer_value},${char_value}`;
}
const testProcessIntegerAndChar = () => {
    console.assert(process_integer_and_char(234, 'H') === '234,H', 'Test 1 Failed');
    console.assert(process_integer_and_char(123, 'A') === '123,A', 'Test 2 Failed');
    console.assert(process_integer_and_char(0, 'Z') === '0,Z', 'Test 3 Failed');
}

testProcessIntegerAndChar();