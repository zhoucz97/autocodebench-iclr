<?php

/**
 * Identifies the most frequently occurring character in a given string.
 * The string is composed of lowercase letters from 'a' to 'z'.
 * If there are multiple characters with the same highest frequency,
 * the character with the smallest ASCII value is returned.
 * 
 * Example:
 * Input: "apple"
 * Output: "p 2" (The character 'p' appears twice)
 * 
 * Input: "environment"
 * Output: "n 3" (The character 'n' appears three times, the most frequent)
 */

function findMostFrequentChar($string) {

    $charCounts = array();
    
    // Count the frequency of each character
    for ($i = 0; $i < strlen($string); $i++) {
        $char = $string[$i];
        if (isset($charCounts[$char])) {
            $charCounts[$char]++;
        } else {
            $charCounts[$char] = 1;
        }
    }
    
    $maxCount = 0;
    $resultChar = '';
    
    // Find the character with the highest frequency
    foreach ($charCounts as $char => $count) {
        if ($count > $maxCount) {
            $maxCount = $count;
            $resultChar = $char;
        } elseif ($count == $maxCount) {
            // If counts are equal, choose the one with smaller ASCII value
            if (ord($char) < ord($resultChar)) {
                $resultChar = $char;
            }
        }
    }
    
    return $resultChar . " " . $maxCount;
}

// Test cases
function main(){
    assert(findMostFrequentChar("abbccc") === "c 3");
    assert(findMostFrequentChar("adfadffasdf") === "f 4");
    assert(findMostFrequentChar("environment") === "n 3");
    assert(findMostFrequentChar("apple") === "p 2");
    assert(findMostFrequentChar("banana") === "a 3");
    
}


main();
?>