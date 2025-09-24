<?php


/**
 * Calculates the minimum number of apples required to be distributed among children.
 * Each child should receive a different number of apples and every child must receive at least one apple.
 * The total count of children is a positive integer not greater than 1000.
 * Input: An integer representing the number of children.
 * Output: An integer representing the minimum number of apples required.
 */

 function minimumApplesForChildren(int $childrenCount): int {

    return $childrenCount * ($childrenCount + 1) / 2;
}
function main(){
    assert(minimumApplesForChildren(1) === 1);
    assert(minimumApplesForChildren(2) === 3);
    assert(minimumApplesForChildren(3) === 6);
    assert(minimumApplesForChildren(8) === 36);
    assert(minimumApplesForChildren(10) === 55);
    
}


main();
?>