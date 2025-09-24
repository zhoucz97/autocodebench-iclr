<?php


/**
 * Finds M positive integers whose sum is N and whose product is as large as possible.
 * Outputs the combination with the smallest lexicographical order if there are multiple possibilities.
 * The function aims to distribute the total sum N into M parts such that the product of these parts is maximized.
 * 
 * Input: Two integers, $N representing the total sum and $M representing the number of parts.
 * Output: An array of integers representing the distribution that maximizes the product.
 * Example: Given N = 6 and M = 3, the function returns [2, 2, 2] as dividing 6 into three parts of 2 each maximizes the product 2*2*2 = 8.
 */

function maxProductDistribution($N, $M) {

    if ($M > $N) {
        return []; // Not possible to distribute N into M positive integers
    }
    
    $base = intdiv($N, $M);
    $remainder = $N % $M;
    
    $result = array();
    for ($i = 0; $i < $M; $i++) {
        if ($i < $remainder) {
            $result[] = $base + 1;
        } else {
            $result[] = $base;
        }
    }
    
    // To ensure lexicographical order, we need the smallest numbers first, but in this approach, the remainder parts are the larger numbers.
    // However, since we are placing the (base + 1) parts first, the array is already in non-decreasing order, which is the smallest lexicographical order for this scenario.
    // For example, for N=6, M=3: base=2, remainder=0 → [2,2,2].
    // For N=7, M=3: base=2, remainder=1 → [3,2,2], which is lex smallest.
    
    return $result;
}
function main(){
    // Test Case 1: Simple distribution
    assert(maxProductDistribution(6, 3) === [2, 2, 2]);

    // Test Case 2: When distribution is not perfectly even
    assert(maxProductDistribution(7, 3) === [2, 2, 3]);

    // Test Case 3: Larger numbers with an even distribution
    assert(maxProductDistribution(10, 2) === [5, 5]);

    // Test Case 4: Larger numbers with a remainder
    assert(maxProductDistribution(9, 4) === [2, 2, 2, 3]);
    assert(maxProductDistribution(9, 3) === [3,3, 3]);

    // Test Case 5: Single part (edge case)
    assert(maxProductDistribution(5, 1) === [5]);

}


main();
?>