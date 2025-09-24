<?php

/**
 * Determines the exact price of an item within a specified range based on a series of hints.
 * The item's price is within the range [1, maxPrice].
 * Hints are given in a string where:
 * - 'u' indicates the actual price is higher than the current guess.
 * - 'd' indicates the actual price is lower than the current guess.
 * - 'y' indicates the current guess is correct.
 * The function uses a binary search approach to narrow down the possible price.
 * Input: The maximum price (maxPrice) and a string of hints.
 * Output: Returns the exact price of the item.
 * Example: For maxPrice = 1000000 and hints = "uddy", the function returns 562500.
 */

function guessItemPrice(int $maxPrice, string $hints): int {

    $low = 1;
    $high = $maxPrice;
    
    for ($i = 0; $i < strlen($hints); $i++) {
        $mid = intval(($low + $high) / 2);
        $hint = $hints[$i];
        
        if ($hint == 'y') {
            return $mid;
        } elseif ($hint == 'u') {
            $low = $mid + 1;
        } elseif ($hint == 'd') {
            $high = $mid - 1;
        }
    }
    
    // If no 'y' hint was encountered, return the final midpoint
    return intval(($low + $high) / 2);
}
function main(){
    assert(guessItemPrice(1000000, "uddy") === 562500);
    assert(guessItemPrice(500, "ududy") === 327);
    assert(guessItemPrice(100, "uuudy") === 91);
    assert(guessItemPrice(1000, "ddduy") === 93);
    
}


main();
?>