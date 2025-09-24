<?php

/**
 * Determines if there exists at least one permutation of adding '+' or '-' 
 * before each number in a sequence such that the sum of the sequence is 
 * divisible by a given number k.
 * 
 * The function takes two arguments: 
 * - $numbers: An array of positive integers representing the sequence.
 * - $k: An integer representing the divisor.
 * 
 * The function returns "YES" if at least one permutation of the sequence 
 * can be divided by $k, otherwise returns "NO".
 * 
 * Example:
 * Given $numbers = [1, 2, 4] and $k = 2, the function will return "NO".
 * Given $numbers = [1, 2, 4] and $k = 3, the function will return "YES".
 */

 function canBeDivided(array $numbers, int $k): string {

    $n = count($numbers);
    if ($k == 0) {
        return "NO"; // division by zero is undefined, but per problem statement, k is given as integer, likely positive
    }
    
    // Initialize a DP array where dp[i][j] indicates whether the first i numbers can achieve sum mod k = j
    // Using a set for each step to track possible remainders
    $dp = array_fill(0, $n + 1, array());
    $dp[0][0] = true; // base case: 0 numbers sum to 0
    
    for ($i = 1; $i <= $n; $i++) {
        $current_num = $numbers[$i - 1];
        $new_remainders = array();
        
        foreach ($dp[$i - 1] as $prev_remainder => $exists) {
            if ($exists) {
                // Add current number
                $new_remainder_add = ($prev_remainder + $current_num) % $k;
                if ($new_remainder_add < 0) {
                    $new_remainder_add += $k;
                }
                $new_remainders[$new_remainder_add] = true;
                
                // Subtract current number
                $new_remainder_sub = ($prev_remainder - $current_num) % $k;
                if ($new_remainder_sub < 0) {
                    $new_remainder_sub += $k;
                }
                $new_remainders[$new_remainder_sub] = true;
            }
        }
        $dp[$i] = $new_remainders;
    }
    
    // Check if the last step has remainder 0
    return isset($dp[$n][0]) ? "YES" : "NO";
}
function main(){
    assert(canBeDivided([1, 2, 4], 2) === "NO");
    assert(canBeDivided([1, 2, 4], 3) === "YES");
    assert(canBeDivided([1, 2, 3], 6) === "YES");
    assert(canBeDivided([5, 10, 15], 7) === "YES");
    assert(canBeDivided([10, 20, 30], 10) === "YES");
    
}


main();
?>