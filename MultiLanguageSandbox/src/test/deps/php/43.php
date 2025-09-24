<?php

/**
 * Finds the length of the longest sequence of consecutive numbers in an array.
 * Consecutive numbers are numbers which come one after the other in sequence without any gaps.
 * For example, in the array [1, 3, 5, 4, 2, 6], the longest sequence of consecutive numbers is [1, 2, 3, 4, 5, 6], which has a length of 6.
 * Input: An array of integers.
 * Output: The length of the longest sequence of consecutive numbers found in the array.
 */
function findLongestConsecutiveSequence(array $numbers): int {

    if (empty($numbers)) {
        return 0;
    }
    
    $numberSet = array_flip($numbers); // Using array_flip for O(1) lookups
    $maxLength = 0;
    
    foreach ($numberSet as $num => $value) {
        // Only start counting if the number is the beginning of a sequence
        if (!isset($numberSet[$num - 1])) {
            $currentNum = $num;
            $currentLength = 1;
            
            // Count how many consecutive numbers follow
            while (isset($numberSet[$currentNum + 1])) {
                $currentNum++;
                $currentLength++;
            }
            
            $maxLength = max($maxLength, $currentLength);
        }
    }
    
    return $maxLength;
}
function main(){
    assert(findLongestConsecutiveSequence([1, 5, 6, 2, 3, 4, 5, 6, 8, 9]) === 6);
    assert(findLongestConsecutiveSequence([10, 4, 20, 1, 3, 2]) === 4);
    assert(findLongestConsecutiveSequence([1, 2, 2, 3]) === 3);
    assert(findLongestConsecutiveSequence([]) === 0);
    assert(findLongestConsecutiveSequence([7]) === 1);
    
}


main();
?>