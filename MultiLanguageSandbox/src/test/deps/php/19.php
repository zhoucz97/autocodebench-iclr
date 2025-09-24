<?php

/**
 * Calculates the total effort required to sort a list of disordered elements.
 * In this scenario, imagine a series of numbered containers in a warehouse. Each container must be arranged in ascending order. 
 * The effort to swap two containers is proportional to the distance between them, measured as double their positional difference.
 * Input: An array of integers representing the unsorted container numbers.
 * Output: Returns an integer representing the total effort required to sort the containers.
 * Example: For input [3, 1, 4, 2], the output is 8.
 */

function calculateEffort($horseNumbers) {

    $sorted = $horseNumbers;
    sort($sorted);
    $effort = 0;
    $n = count($horseNumbers);
    
    // Create a map from value to its index in the sorted array
    $valueToSortedIndex = array();
    foreach ($sorted as $index => $value) {
        $valueToSortedIndex[$value] = $index;
    }
    
    for ($i = 0; $i < $n; $i++) {
        $currentValue = $horseNumbers[$i];
        $correctIndex = $valueToSortedIndex[$currentValue];
        $effort += 2 * abs($i - $correctIndex);
    }
    
    return $effort;
}
function main(){
    assert(calculateEffort([2, 3, 6, 5, 8, 4]) === 14);
    assert(calculateEffort([3, 1, 4, 2]) === 8);
    assert(calculateEffort([1, 2, 3, 4]) === 0); // Already sorted
    assert(calculateEffort([4, 3, 2, 1]) === 20); // Reverse order
    
}


main();
?>