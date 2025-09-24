<?php
/**
    * Calculates the sum of all positive integers less than or equal to a given number N that are not 'related' to the number 2.
    * A number is 'related' to 2 if it is divisible by 2 or if any of its digits is 2.
    * For example, for N = 8, the numbers not related to 2 are 1, 3, 5, and 7, and their sum is 16.
    * Input: An integer N.
    * Output: Returns the sum of all integers less than or equal to N that are not related to 2.
*/

function sumOfUnrelatedToTwo($N) {

    $sum = 0;
    
    for ($i = 1; $i <= $N; $i++) {
        // Check if the number is divisible by 2
        if ($i % 2 == 0) {
            continue;
        }
        
        // Check if any digit in the number is 2
        $num = $i;
        while ($num > 0) {
            $digit = $num % 10;
            if ($digit == 2) {
                break;
            }
            $num = (int)($num / 10);
        }
        
        // If neither condition is met, add to sum
        if ($num == 0) {
            $sum += $i;
        }
    }
    
    return $sum;
}

// Example usage:
function main(){
    assert(sumOfUnrelatedToTwo(8) === 16);  // 1 + 3 + 5 + 7
    assert(sumOfUnrelatedToTwo(10) === 25); // 1 + 3 + 5 + 7 + 9
    assert(sumOfUnrelatedToTwo(2) === 1);   // 1
    assert(sumOfUnrelatedToTwo(22) === 100); // Sum of numbers not containing 2 and not even, up to 22
    assert(sumOfUnrelatedToTwo(0) === 0);   // No numbers to sum
    
}


main();
?>