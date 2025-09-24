// Note: No import statement is required in JavaScript for this function
/**
 * Checks if a string with exactly three characters has two characters that are the same.
 * 
 * @param {string} str - The three-character string to be checked.
 * @return {string} - Returns 'Yes' if the input has exactly two equal characters, otherwise 'No'.
 * 
 * Examples:
 * checkTwoEqualDigits('112') // 'Yes'
 * checkTwoEqualDigits('123') // 'No'
 */

function checkTwoEqualDigits(str) {
    // Check if the string length is exactly 3
    if (str.length !== 3) {
        return 'No';
    }
    
    // Check for exactly two equal characters
    if ((str[0] === str[1] && str[0] !== str[2]) || 
        (str[0] === str[2] && str[0] !== str[1]) || 
        (str[1] === str[2] && str[1] !== str[0])) {
        return 'Yes';
    } else {
        return 'No';
    }
}
const testCheckTwoEqualDigits = () => {
    console.assert(checkTwoEqualDigits("112") === "Yes");
    console.assert(checkTwoEqualDigits("123") === "No");
    console.assert(checkTwoEqualDigits("232") === "Yes");
    console.assert(checkTwoEqualDigits("444") === "No"); // All three characters are equal, not two.
    console.assert(checkTwoEqualDigits("121") === "Yes");

    // Additional test cases to cover more scenarios
    console.assert(checkTwoEqualDigits("787") === "Yes");
    console.assert(checkTwoEqualDigits("999") === "No"); // All three characters are equal
    console.assert(checkTwoEqualDigits("890") === "No");
    console.assert(checkTwoEqualDigits("556") === "Yes");
    console.assert(checkTwoEqualDigits("353") === "Yes"); // No two characters are equal
    
    // console.log("All tests passed successfully.");
};

testCheckTwoEqualDigits();