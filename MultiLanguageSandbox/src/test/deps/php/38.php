<?php

/**
 * Question:
 * You have M identical apples that need to be placed into N identical plates. You are allowed
 * to have some plates empty. How many different ways are there to distribute the apples into the plates?
 * 5,1,1 and 1,5,1 are the same division method.
 *
 * Example:
 * - Input: M = 7, N = 3
 * - Output: 8

 */

 function countDistributionMethods($M, $N) {

    // Create a DP table initialized to 0
    $dp = array_fill(0, $M + 1, array_fill(0, $N + 1, 0));
    
    // Base case: 1 way to distribute 0 apples (all plates empty)
    for ($j = 0; $j <= $N; $j++) {
        $dp[0][$j] = 1;
    }
    
    // Fill the DP table
    for ($i = 1; $i <= $M; $i++) {
        for ($j = 1; $j <= $N; $j++) {
            // Option 1: at least one plate has at least one apple
            // This is equivalent to placing at least one apple in the first plate
            // and then distributing the remaining (i - k) apples into j plates,
            // but to avoid overcounting, we ensure the first plate's count is >= the next plates'
            // So we can think of it as placing 'k' apples in the first plate (k >=1),
            // then the remaining (i -k) apples must be distributed into j plates with each plate <=k.
            // But a better approach is to realize that the problem reduces to partitioning i into parts of size <= min(j, i)
            // So the recurrence is dp[i][j] = dp[i][j-1] (no plates with k apples) + dp[i -k][j] for k from 1 to i/j
            // But to optimize, we can use the following:
            $dp[$i][$j] = $dp[$i][$j - 1]; // plates with 0 apples
            if ($i >= $j) {
                $dp[$i][$j] += $dp[$i - $j][$j];
            }
        }
    }
    
    return $dp[$M][$N];
}
function main(){
    // Test cases
    assert(countDistributionMethods(7, 3) === 8);
    assert(countDistributionMethods(5, 2) === 3);
    assert(countDistributionMethods(10, 5) === 30);

}


main();
?>