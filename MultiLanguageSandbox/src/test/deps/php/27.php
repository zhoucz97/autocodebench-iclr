<?php

/**
    * Finds the smallest integer divisor greater than 1 that yields the same remainder when dividing three given numbers.
    * This problem involves finding the minimum value of x (where x > 1) such that when x divides each of the 
    * three integers a, b, and c, the remainders are the same.
    * Input: Three positive integers a, b, and c, each not exceeding 1,000,000.
    * Output: Returns the smallest integer value of x that satisfies the condition.
    * Example: For a = 300, b = 262, c = 205, the function returns 19.
*/

function findMinimumDivisor($a, $b, $c){

    // Calculate the absolute differences
    $d1 = abs($a - $b);
    $d2 = abs($a - $c);
    $d3 = abs($b - $c);
    
    // Compute the GCD of the three differences
    $gcd = gcd($d1, $d2);
    $gcd = gcd($gcd, $d3);
    
    // If the GCD is 1, there's no solution (but problem says there is)
    // So we need to find the smallest divisor of the GCD greater than 1
    if ($gcd == 1) {
        return -1; // though problem says there's a solution, so maybe input constraints ensure gcd >1
    }
    
    // Find the smallest divisor of gcd greater than 1
    for ($i = 2; $i * $i <= $gcd; $i++) {
        if ($gcd % $i == 0) {
            return $i;
        }
    }
    return $gcd; // if gcd is a prime number, then gcd itself is the answer
}

function gcd($a, $b) {
    while ($b != 0) {
        $temp = $a % $b;
        $a = $b;
        $b = $temp;
    }
    return $a;
}
function main(){
    assert(findMinimumDivisor(300, 262, 205) === 19);
    assert(findMinimumDivisor(100, 200, 300) === 2);
    assert(findMinimumDivisor(15, 25, 35) === 2);
    assert(findMinimumDivisor(7, 14, 21) === 7);
    assert(findMinimumDivisor(9, 18, 27) === 3);
    
}


main();
?>