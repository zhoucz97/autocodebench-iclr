<?php

/**
    * Calculates the number of different ways to exchange a paper bill into coins.
    * Given a paper bill of value N (1 <= N <= 10), there are two types of coins available: 1 and 2 units.
    * The function determines how many distinct combinations of these coins can sum up to the value of the bill.
    * Example: For a bill of value 4, there are 5 ways:
    * 4 = 1 + 1 + 1 + 1
    * 4 = 2 + 1 + 1
    * 4 = 1 + 2 + 1
    * 4 = 1 + 1 + 2
    * 4 = 2 + 2
    * Input: An integer representing the bill value.
    * Output: Returns the number of different ways to exchange the bill into coins.
*/
function countCoinExchangeWays($billValue) {

    if ($billValue == 1) {
        return 1;
    }
    
    $dp = array_fill(0, $billValue + 1, 0);
    $dp[0] = 1; // Base case: one way to make 0 (using no coins)
    
    for ($i = 1; $i <= $billValue; $i++) {
        if ($i >= 1) {
            $dp[$i] += $dp[$i - 1];
        }
        if ($i >= 2) {
            $dp[$i] += $dp[$i - 2];
        }
    }
    
    return $dp[$billValue];
}
function main(){
    assert(countCoinExchangeWays(4) === 5);
    assert(countCoinExchangeWays(3) === 3);
    assert(countCoinExchangeWays(2) === 2);
    assert(countCoinExchangeWays(1) === 1);
}

main();
?>