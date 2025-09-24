
/**
 * Checks if a three-character string has exactly two characters that are the same.
 *
 * @param str The three-character string to be checked.
 * @return Returns 'Yes' if the input has exactly two equal characters, otherwise 'No'.
 * 
 * Examples:
 *  checkTwoEqualDigits("112") // Returns 'Yes'
 */

function checkTwoEqualDigits(str: string): string {
    if (str.length !== 3) {
        return 'No';
    }
    
    const [a, b, c] = str.split('');
    
    // Check if exactly two characters are the same
    if ((a === b && a !== c) || (a === c && a !== b) || (b === c && b !== a)) {
        return 'Yes';
    } else {
        return 'No';
    }
}
function testCheckTwoEqualDigits(): void {
    // Testing the logic with various cases.
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

    // Uncomment the next line if you wish to print to console upon successful tests
    //console.log("All tests passed successfully.");
}

// Run the tests
testCheckTwoEqualDigits();