<?php


/**
 * Finds the nth digit after the decimal point in the decimal representation of the fraction a/b.
 * The function takes three integers as input: a (numerator), b (denominator), and n (position of the digit after the decimal point).
 * Conditions: 0 < a < b < 100, 1 <= n <= 10000.
 * The function calculates the decimal representation of a/b and returns the nth digit after the decimal point.
 * 
 * Example:
 * Input: a = 1, b = 2, n = 1
 * Output: 5 (since 1/2 = 0.5)
 */

 function getDecimalDigit($a, $b, $n): int{

    $remainder = $a % $b;
    for ($i = 1; $i <= $n; $i++) {
        $remainder *= 10;
        $digit = (int)($remainder / $b);
        $remainder = $remainder % $b;
        if ($i == $n) {
            return $digit;
        }
    }
    return -1; // This line is theoretically unreachable given the constraints
}
function main(){
    assert(getDecimalDigit(1, 2, 1) === 5);
    assert(getDecimalDigit(1, 3, 1) === 3);
    assert(getDecimalDigit(1, 4, 2) === 5);
    assert(getDecimalDigit(3, 7, 3) === 8);
    assert(getDecimalDigit(50, 99, 4) === 0);   
}


main();
?>