<?php
/**
 * Solves the Josephus Problem.
 * In this problem, N people are arranged in a circle and eliminated in every Mth position
 * until only one person remains. The function simulates this process and returns the order
 * in which people are eliminated.
 * 
 * For example, with N = 8 and M = 5, the elimination order is 5, 2, 8, 7, 1, 4, 6, 3.
 * 
 * Input: Two integers, N (number of people in the circle) and M (the elimination step).
 * Output: Returns an array representing the order in which people are eliminated.
 */

function josephusProblem($n, $m) {

    // Create an array representing the circle of people (1 to n)
    $people = range(1, $n);
    $eliminationOrder = [];
    $currentIndex = 0;

    while (count($people) > 1) {
        // Calculate the index of the person to eliminate
        $currentIndex = ($currentIndex + $m - 1) % count($people);
        
        // Add the eliminated person to the result array
        $eliminated = $people[$currentIndex];
        array_splice($people, $currentIndex, 1);
        $eliminationOrder[] = $eliminated;
    }

    // The last remaining person is also added to the elimination order
    $eliminationOrder[] = $people[0];

    return $eliminationOrder;
}

// Example usage:
$result = josephusProblem(8, 5);
print_r($result); // Output: Array ( [0] => 5 [1] => 2 [2] => 8 [3] => 7 [4] => 1 [5] => 4 [6] => 6 [7] => 3 )
function main(){
    assert(josephusProblem(8, 5) === [5, 2, 8, 7, 1, 4, 6, 3]);
    assert(josephusProblem(4, 2) === [2, 4, 3, 1]);
    assert(josephusProblem(6, 3) === [3, 6, 4, 2, 5, 1]);    
}


main();
?>