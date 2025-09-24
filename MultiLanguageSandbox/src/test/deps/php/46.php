<?php


/**
 * Calculates the number of characters in a composition's title.
 * The title may contain uppercase and lowercase English letters, digits, spaces, and newline characters.
 * The function counts the characters excluding spaces and newline characters.
 * 
 * Example usage:
 * - For a title "234", the function returns 3.
 * - For a title "Ca 45\n", including a newline character, the function returns 4.
 * 
 * @param string $title The title of the composition.
 * @return int The number of characters in the title, excluding spaces and newline characters.
 */

function countTitleCharacters(string $title): int {

    // Remove all spaces and newline characters from the title
    $cleanedTitle = str_replace([' ', "\n"], '', $title);
    // Return the length of the cleaned title
    return strlen($cleanedTitle);
}
function main(){
    assert(countTitleCharacters("234") === 3, 'Testcase 1 failed');
    assert(countTitleCharacters("Ca 45") === 4, 'Testcase 2 failed');
    assert(countTitleCharacters(" \nCa 45\n ") === 4, 'Testcase 3 failed');
    assert(countTitleCharacters("") === 0, 'Testcase 5 failed');
    
}


main();
?>