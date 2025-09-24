
/**
 * Transforms the case of a given letter.
 * - letter (char): The input letter to be transformed.
 * 
 * Returns:
 * - char: The letter with its case reversed. If the input is lowercase,
 * returns uppercase, and if the input is uppercase, returns lowercase.
 * 
 * Example:
 * transformLetterCase('b') returns 'B'
 * transformLetterCase('Z') returns 'z'
 */

function transformLetterCase(letter) {
    if (letter === letter.toLowerCase() && letter !== letter.toUpperCase()) {
        return letter.toUpperCase();
    } else if (letter === letter.toUpperCase() && letter !== letter.toLowerCase()) {
        return letter.toLowerCase();
    }
    return letter; // Return as-is if it's not a letter (though the problem implies it is)
}
const testTransformLetterCase = () => {
    console.assert(transformLetterCase('b') === 'B');
    console.assert(transformLetterCase('B') === 'b');
    // Additional test cases
    console.assert(transformLetterCase('z') === 'Z');
    console.assert(transformLetterCase('Z') === 'z');
    console.assert(transformLetterCase('m') === 'M');
    console.assert(transformLetterCase('M') === 'm');
    console.assert(transformLetterCase('1') === '1'); // Non-alphabetic, should be unchanged
    console.assert(transformLetterCase('!') === '!'); // Non-alphabetic, should be unchanged

    // Uncomment the following line to log a message for passed tests
    // console.log("All tests passed successfully.");
};

testTransformLetterCase();