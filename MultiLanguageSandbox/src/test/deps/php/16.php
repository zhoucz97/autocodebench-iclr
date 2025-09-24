<?php

/**
 * Calculates the maximum value of a segment from a given array of values.
 * Imagine a tailor has a ribbon with various segments, each having a different value, positive or negative.
 * The goal is to find the maximum total value that can be obtained by cutting a continuous segment of the ribbon.
 * This is akin to finding the most valuable portion of the ribbon to sell or use.
 *
 * Input: 
 * An array $values of integers representing the value of each segment of the ribbon.
 *
 * Output:
 * Returns an integer representing the maximum value that can be obtained from any continuous segment of the ribbon.
 *
 * Example:
 * For a ribbon of length 4 with values [-1, 3, 2, -4], the function should return 5, 
 * as the segment [3, 2] gives the maximum value.
 */

 function maxSegmentValue($values){

    if (empty($values)) {
        return 0;
    }
    
    $max_current = $values[0];
    $max_global = $values[0];
    
    for ($i = 1; $i < count($values); $i++) {
        $max_current = max($values[$i], $max_current + $values[$i]);
        if ($max_current > $max_global) {
            $max_global = $max_current;
        }
    }
    
    return $max_global;
}
function main(){
    assert(maxSegmentValue([-1, 3, 2, -4]) === 5);
    assert(maxSegmentValue([-2, -3, 4, -1, -2, 1, 5, -3]) === 7);
    assert(maxSegmentValue([-1, -2, -3]) === -1);
    assert(maxSegmentValue([10]) === 10);
    assert(maxSegmentValue([1, -3]) === 1);
    
    
}


main();
?>