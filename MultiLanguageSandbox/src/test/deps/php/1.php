<?php
assert_options(ASSERT_ACTIVE, 1);
assert_options(ASSERT_BAIL, 1);
/**
    * Check if in given list of numbers, any two numbers are closer to each other than
    * given threshold.
*/
function has_close_elements($numbers, $threshold) {

    // First, sort the array to easily find the closest elements
    sort($numbers);
    
    // Iterate through the sorted array and check adjacent elements
    for ($i = 0; $i < count($numbers) - 1; $i++) {
        $current = $numbers[$i];
        $next = $numbers[$i + 1];
        
        // Calculate the absolute difference between current and next element
        $difference = abs($current - $next);
        
        // If the difference is less than the threshold, return true
        if ($difference < $threshold) {
            return true;
        }
    }
    
    // If no such pair is found, return false
    return false;
}
// Test cases
function test_has_close_elements() {
    if (has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3) != true){
        exit(1);
    }

    if (has_close_elements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05) != false){
        exit(1);
    }
    if (has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95) != true){
        exit(1);
    }
    if (has_close_elements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8) != false){
        exit(1);
    }
    if (has_close_elements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1) != true){
        exit(1);
    }
    if (has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0) != true){
        exit(1);
    }
    if (has_close_elements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5) != false){
        exit(1);
    }
}

test_has_close_elements();

?>