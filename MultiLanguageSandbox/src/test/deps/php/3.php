<?php

/**
    * Removes all asterisks (*) from a string except those at the beginning.
    * This function takes a string that may contain alphabetic characters and asterisks.
    * It will preserve any asterisks that appear at the start of the string but remove all others.
    * For example, a string like "***Hello*World**" would be transformed to "***HelloWorld".
    * Input: A single string.
    * Output: The modified string with only the leading asterisks retained.
*/
function removeInnerAsterisks($string) {

    // Find the position where the first non-asterisk character appears
    $firstNonAsteriskPos = 0;
    while ($firstNonAsteriskPos < strlen($string) && $string[$firstNonAsteriskPos] == '*') {
        $firstNonAsteriskPos++;
    }
    
    // Preserve the leading asterisks and remove all others
    $leadingAsterisks = substr($string, 0, $firstNonAsteriskPos);
    $restOfString = substr($string, $firstNonAsteriskPos);
    $restWithoutAsterisks = str_replace('*', '', $restOfString);
    
    return $leadingAsterisks . $restWithoutAsterisks;
}
function main(){
    assert(removeInnerAsterisks("***Hello*World**") === "***HelloWorld");
    assert(removeInnerAsterisks("****") === "****");
    assert(removeInnerAsterisks("abc*def**ghi") === "abcdefghi");
    assert(removeInnerAsterisks("*a*b*c*") === "*abc");
    assert(removeInnerAsterisks("") === "");
    
}

main();
?>