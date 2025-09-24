<?php

/**
 * Performs division of two large numbers provided as strings.
 * This function handles division of large numbers that may exceed the integer
 * or float limit in PHP. It only returns the integer part of the quotient.
 * 
 * Inputs:
 * - $dividend: String representing the dividend (the number to be divided).
 * - $divisor: String representing the divisor (the number by which we divide).
 * 
 * Output:
 * - Returns a string representing the integer part of the quotient.
 * 
 * Example Cases:
 * - largeNumberDivision("2376", "24") returns "99"
 * - largeNumberDivision("1000", "10") returns "100"
 */

 function largeNumberDivision($dividend, $divisor) {

    // Handle division by zero
    if ($divisor === "0") {
        throw new InvalidArgumentException("Division by zero is not allowed");
    }
    
    // If dividend is zero, return zero
    if ($dividend === "0") {
        return "0";
    }
    
    // Remove leading zeros from both numbers
    $dividend = ltrim($dividend, '0');
    $divisor = ltrim($divisor, '0');
    
    // If after trimming, dividend is empty, it was all zeros
    if ($dividend === "") {
        return "0";
    }
    
    // If divisor is empty after trimming, it was all zeros (already handled above)
    
    // Compare lengths to determine if result is zero
    if (strlen($dividend) < strlen($divisor)) {
        return "0";
    }
    
    $quotient = "";
    $remainder = "0";
    
    // Process each digit of the dividend
    for ($i = 0; $i < strlen($dividend); $i++) {
        $currentDigit = $dividend[$i];
        $remainder = $remainder . $currentDigit;
        
        // Remove leading zeros from remainder
        $remainder = ltrim($remainder, '0');
        if ($remainder === "") {
            $remainder = "0";
        }
        
        // Compare remainder with divisor
        if (compareNumbers($remainder, $divisor) >= 0) {
            $count = 0;
            while (compareNumbers($remainder, $divisor) >= 0) {
                $remainder = subtractNumbers($remainder, $divisor);
                $count++;
            }
            $quotient .= $count;
        } else {
            if ($quotient !== "" || $i === 0) { // Only add zero if quotient isn't empty or it's the first digit
                $quotient .= "0";
            }
        }
    }
    
    // Remove leading zeros from the quotient
    $quotient = ltrim($quotient, '0');
    
    // If quotient is empty after trimming, return "0"
    return $quotient === "" ? "0" : $quotient;
}

// Helper function to compare two numbers represented as strings
function compareNumbers($a, $b) {
    // Remove leading zeros
    $a = ltrim($a, '0');
    $b = ltrim($b, '0');
    
    // If either is empty after trimming, it was zero
    if ($a === "") $a = "0";
    if ($b === "") $b = "0";
    
    // Compare lengths
    $lenA = strlen($a);
    $lenB = strlen($b);
    
    if ($lenA > $lenB) {
        return 1;
    } elseif ($lenA < $lenB) {
        return -1;
    } else {
        // Same length, compare digit by digit
        for ($i = 0; $i < $lenA; $i++) {
            if ($a[$i] > $b[$i]) {
                return 1;
            } elseif ($a[$i] < $b[$i]) {
                return -1;
            }
        }
        return 0; // Numbers are equal
    }
}

// Helper function to subtract two numbers represented as strings
function subtractNumbers($a, $b) {
    // Remove leading zeros
    $a = ltrim($a, '0');
    $b = ltrim($b, '0');
    
    // If either is empty after trimming, it was zero
    if ($a === "") $a = "0";
    if ($b === "") $b = "0";
    
    // Make sure $a is the larger number
    if (compareNumbers($a, $b) < 0) {
        return "0"; // Result would be negative, but we're only doing positive division
    }
    
    $result = "";
    $borrow = 0;
    $lenA = strlen($a);
    $lenB = strlen($b);
    
    // Pad the shorter number with leading zeros
    $b = str_pad($b, $lenA, '0', STR_PAD_LEFT);
    
    for ($i = $lenA - 1; $i >= 0; $i--) {
        $digitA = (int)$a[$i];
        $digitB = (int)$b[$i];
        
        $digitA -= $borrow;
        if ($digitA < $digitB) {
            $digitA += 10;
            $borrow = 1;
        } else {
            $borrow = 0;
        }
        
        $resultDigit = $digitA - $digitB;
        $result = $resultDigit . $result;
    }
    
    // Remove leading zeros from result
    $result = ltrim($result, '0');
    
    return $result === "" ? "0" : $result;
}
function main(){
    assert(largeNumberDivision("2376", "24") === "99");
    assert(largeNumberDivision("1000", "10") === "100");
    assert(largeNumberDivision("5000", "25") === "200");
    assert(largeNumberDivision("123456789", "12345") === "10000");
    assert(largeNumberDivision("102030", "5101") === "20");
    assert(largeNumberDivision("1020300000000000000000000000000000000000000000000", "10") === "102030000000000000000000000000000000000000000000");
    
}


main();
?>