<?php


/**
 * Calculates the largest integer x such that ax <= b < ax+1 or x <= logab < x + 1.
 * This function helps in understanding the relationship between two numbers in terms of logarithms and exponents.
 * - The function takes two positive integers 'a' and 'b' as inputs.
 * - It finds the largest integer 'x' that satisfies the conditions mentioned above.
 * 
 * Args:
 *      $a (int): The base number, must be greater than 1.
 *      $b (int): The comparison number.
 * 
 * Returns:
 *      int: The largest integer x satisfying the conditions.
 * 
 * Examples:
 *      - If a = 10000 and b = 1000000000001, then the output should be 3.
 *      - If a = 3 and b = 27, then the output should be 3.
 */

function calculateLogLimit($a, $b) {

    // Ensure that 'a' is greater than 1 and both 'a' and 'b' are positive integers
    if ($a <= 1 || $b <= 0) {
        throw new InvalidArgumentException("Both 'a' must be greater than 1 and 'b' must be a positive integer.");
    }

    // Calculate the logarithm of 'b' with base 'a'
    $logValue = log($b, $a);

    // Find the largest integer x such that x <= logValue < x + 1
    $x = floor($logValue);

    return $x;
}

// Example usage:
function main(){
    assert(calculateLogLimit(10000, 1000000000001) === 3);
    assert(calculateLogLimit(3, 27) === 3);
    assert(calculateLogLimit(2, 32) === 5);
    assert(calculateLogLimit(5, 3125) === 5);
   
    
}

main();
?>