<?php


/**
 * Counts the number of digits, lowercase letters, and uppercase letters in an 8-character string.
 *
 * This function takes a single string of exactly 8 characters as input.
 * It then counts and returns the number of digits, lowercase letters, and uppercase letters within the string.
 *
 * Input: A string of 8 characters.
 * Output: An array of three integers, where:
 * - The first integer is the count of digits in the string.
 * - The second integer is the count of lowercase letters in the string.
 * - The third integer is the count of uppercase letters in the string.
 *
 * Examples:
 * - For the string "yLOI2022", the output should be [4, 1, 3].
 * - For the string "IAKIOIOI", the output should be [0, 0, 8].
 * - For the string "1n2s0e1s", the output should be [4, 4, 0].
 */

 function countCharacters($s) {

    $digitCount = 0;
    $lowercaseCount = 0;
    $uppercaseCount = 0;
    
    for ($i = 0; $i < 8; $i++) {
        $char = $s[$i];
        if (ctype_digit($char)) {
            $digitCount++;
        } elseif (ctype_lower($char)) {
            $lowercaseCount++;
        } elseif (ctype_upper($char)) {
            $uppercaseCount++;
        }
    }
    
    return [$digitCount, $lowercaseCount, $uppercaseCount];
}
function main(){
    // Testcases
    assert(countCharacters("yLOI2022") === [4, 1, 3]);
    assert(countCharacters("IAKIOIOI") === [0, 0, 8]);
    assert(countCharacters("1n2s0e1s") === [4, 4, 0]);

    // echo "All test cases passed!";

}


main();
?>