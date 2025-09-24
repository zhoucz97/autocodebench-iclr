<?php

/**
   * Distribute candy to five children sitting around a round table.
   *Each child initially has a certain number of candies. They play a game where, starting with one child, and going clockwise, each child divides their candy into three equal parts, and if there are extra candies that cannot be divided equally, the child eats them. Each child keeps one copy and gives the other two copies to their neighbors. The five children completed the above operations in turn.
   * Input: An array of five integers representing the number of candies each child has.
   * Output: An array of five integers representing the number of candies each child has after playing the game.
   */

 function distributeCandies(array $candies): array{

    // Ensure the input array has exactly 5 elements
    if (count($candies) !== 5) {
        throw new InvalidArgumentException("Input array must contain exactly 5 elements.");
    }

    // Make a copy of the original array to avoid modifying it directly
    $currentCandies = $candies;

    // Iterate through each child in order (0 to 4)
    for ($i = 0; $i < 5; $i++) {
        $childIndex = $i;
        
        // Calculate how many candies the child can divide into three equal parts
        $candiesToDivide = $currentCandies[$childIndex];
        $divided = intdiv($candiesToDivide, 3);
        $remainder = $candiesToDivide % 3;
        
        // The child eats the remainder (if any)
        $currentCandies[$childIndex] = $divided;
        
        // Determine the left and right neighbors (handling circular arrangement)
        $leftNeighbor = ($childIndex - 1 + 5) % 5;
        $rightNeighbor = ($childIndex + 1) % 5;
        
        // Give one part to each neighbor
        $currentCandies[$leftNeighbor] += $divided;
        $currentCandies[$rightNeighbor] += $divided;
    }

    return $currentCandies;
}
function main(){
    $candies = distributeCandies([8, 9, 10, 11, 12]);
    assert($candies === [11, 7, 9, 11, 6]);
    
    
}

main();
?>