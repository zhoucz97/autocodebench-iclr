<?php

/**
 * Determines a pen purchase plan to maximize the number of pens bought with a given amount of class fund.
 * There are three types of pens available at prices of 6, 5, and 4 units respectively.
 * The objective is to buy as many pens as possible without any remaining class fund.
 * 
 * Input: An integer representing the total class fund available.
 * Output: An array with three integers representing the count of pens bought at 6 units, 5 units, and 4 units, in that order.
 * 
 * Example:
 * findPenPurchasePlan(10)
 * Output: [1, 0, 1] (One pen of 6 units and one pen of 4 units)
 */
function findPenPurchasePlan(int $totalFund): array {

    $maxPens = -1;
    $bestCombination = [0, 0, 0];
    
    // Iterate through all possible counts of 6-unit pens
    for ($six = 0; $six * 6 <= $totalFund; $six++) {
        // Iterate through all possible counts of 5-unit pens
        for ($five = 0; $five * 5 <= $totalFund - $six * 6; $five++) {
            $remaining = $totalFund - $six * 6 - $five * 5;
            if ($remaining >= 0 && $remaining % 4 == 0) {
                $four = $remaining / 4;
                $totalPens = $six + $five + $four;
                if ($totalPens > $maxPens) {
                    $maxPens = $totalPens;
                    $bestCombination = [$six, $five, $four];
                }
            }
        }
    }
    
    return $bestCombination;
}
function main(){
    assert(findPenPurchasePlan(10) === [1, 0, 1]);
    assert(findPenPurchasePlan(14) === [1, 0, 2]);
    assert(findPenPurchasePlan(24) === [0, 0, 6]);
    
}

main();
?>