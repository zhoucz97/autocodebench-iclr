
/**
 * Display the ASCII information for a given character.
 * 
 * Parameters:
 * - character (string): The input character for which ASCII information is to be displayed.
 *    
 * Returns:
 * - number: corresponding ASCII code
 *    
 * Examples:
 * ```
 * console.log(display_ascii_info('A'));
 * // expected output: 65
 * ```
 */

function display_ascii_info(character: string): number {
    if (character.length !== 1) {
        throw new Error("Input must be a single character");
    }
    return character.charCodeAt(0);
}

// Example usage:
 // expected output: 65
const testDisplayAsciiInfo = (): void => {
    console.assert(display_ascii_info('A') === 65);
    console.assert(display_ascii_info('B') === 66); // Additional test case
    console.assert(display_ascii_info('0') === 48); // Additional test case
    console.assert(display_ascii_info(' ') === 32); // Additional test case
    // console.log("All tests passed.");
};

testDisplayAsciiInfo();