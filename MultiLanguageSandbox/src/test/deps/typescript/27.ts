
/**
 * Transforms the case of a given letter.
 * 
 * Parameters:
 * - letter (string): The input letter to be transformed.
 * 
 * Returns:
 * - string: The letter with its case reversed. If the input is lowercase, 
 *           it returns the uppercase version, and if the input is uppercase, 
 *           it returns the lowercase version.
 * 
 * Examples:
 * transformLetterCase('b') // Returns 'B'
 */

function transformLetterCase(letter: string): string {
    if (letter.length !== 1) {
        throw new Error("Input must be a single character");
    }
    
    if (letter === letter.toLowerCase() && letter !== letter.toUpperCase()) {
        return letter.toUpperCase();
    } else if (letter === letter.toUpperCase() && letter !== letter.toLowerCase()) {
        return letter.toLowerCase();
    }
    
    return letter; // Return as-is if it's not a letter (or already in both cases)
}
const testTransformLetterCase = (): void => {
    console.assert(transformLetterCase('b') === 'B', 'Test failed: transformLetterCase(\'b\') should return \'B\'');
    console.assert(transformLetterCase('B') === 'b', 'Test failed: transformLetterCase(\'B\') should return \'b\'');
    console.assert(transformLetterCase('z') === 'Z', 'Test failed: transformLetterCase(\'z\') should return \'Z\'');
    console.assert(transformLetterCase('Z') === 'z', 'Test failed: transformLetterCase(\'Z\') should return \'z\'');
    console.assert(transformLetterCase('m') === 'M', 'Test failed: transformLetterCase(\'m\') should return \'M\'');
    console.assert(transformLetterCase('M') === 'm', 'Test failed: transformLetterCase(\'M\') should return \'m\'');
    console.assert(transformLetterCase('1') === '1', 'Test failed: transformLetterCase(\'1\') should return \'1\' (unchanged)');
    console.assert(transformLetterCase('!') === '!', 'Test failed: transformLetterCase(\'!\') should return \'!\' (unchanged)');

    // console.log("All tests passed successfully.");
};

testTransformLetterCase();