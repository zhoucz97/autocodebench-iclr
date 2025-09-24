<?php
/**
 * Converts a decimal number into a sum of powers of 2.
 * This function is useful for understanding the binary representation of a decimal number.
 * It breaks down the number into a sum of powers of 2, which are easier to understand and work with,
 * especially in contexts where binary calculations are frequent.
 * 
 * For example:
 * - Input: 7
 * - Output: "7=4+2+1"
 * - Input: 43
 * - Output: "43=32+8+2+1"
 * 
 * Input: A single integer ($number), where 0 < $number < 1000000.
 * Output: A string representing the equation of the sum of powers of 2 that equals the input number.
 */

function decimalToBinarySum($number) {

    if ($number <= 0 || $number >= 1000000) {
        return "Invalid input. Number must be between 1 and 999999.";
    }

    $powers = array();
    $currentPower = 1;

    while ($currentPower <= $number) {
        if (($number & $currentPower) != 0) {
            array_unshift($powers, $currentPower);
        }
        $currentPower *= 2;
    }

    $sumString = implode('+', $powers);
    return "$number=$sumString";
}
function main(){
    assert(decimalToBinarySum(7) === "7=4+2+1");
    assert(decimalToBinarySum(43) === "43=32+8+2+1");
    assert(decimalToBinarySum(15) === "15=8+4+2+1");
    assert(decimalToBinarySum(1) === "1=1");
    assert(decimalToBinarySum(32) === "32=32");
    
}

main();
?>