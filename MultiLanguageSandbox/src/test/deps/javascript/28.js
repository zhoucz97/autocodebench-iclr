
/**
 * Display the ASCII information for a given character.
 *
 * @param {char} character - The input character for which ASCII information is to be displayed.
 * @returns {number} - The corresponding ASCII code.
 * 
 * Examples:
 * display_ascii_info('A');
 * // => 65
 */

function display_ascii_info(character) {
    // Get the ASCII code of the character
    const asciiCode = character.charCodeAt(0);
    
    // Display the ASCII information (you can modify this part as needed)
    
    
    
    // Return the ASCII code
    return asciiCode;
}

// Example usage:
 // => 65
(() => {
    console.assert(display_ascii_info('A') === 65, "'A' should return 65");
    console.assert(display_ascii_info('B') === 66, "'B' should return 66"); // Additional test case
    console.assert(display_ascii_info('0') === 48, "'0' should return 48"); // Additional test case
    console.assert(display_ascii_info(' ') === 32, "' ' should return 32"); // Additional test case
    // console.log("All tests passed.");
})();