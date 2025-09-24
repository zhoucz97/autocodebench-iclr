<?php

/**
    * Calculates the number of chickens and rabbits in a farm.
    * Given the total number of animals and the total number of legs, the function determines
    * how many chickens and rabbits are there. 
    * Assumptions: 
    * - Each chicken has 2 legs.
    * - Each rabbit has 4 legs.
    * - The total number of animals and legs are non-negative integers.
    * 
    * Input: Two integers, $totalAnimals (total number of animals) and $totalLegs (total number of legs).
    * Output: Returns an array [number_of_chickens, number_of_rabbits] if a valid solution exists, 
    *         or "No answer" if no valid solution is found.
    * 
    * Example:
    * For 5 animals and 12 legs, the function will return [4, 1] (4 chickens and 1 rabbit).
    * For 10 animals and 16 legs, the function will return "No answer" as no valid combination exists.
    */

    function calculateAnimals($totalAnimals, $totalLegs) {

    // Check for invalid inputs (negative numbers)
    if ($totalAnimals < 0 || $totalLegs < 0) {
        return "No answer";
    }
    
    // Iterate through possible number of rabbits
    for ($rabbits = 0; $rabbits <= $totalAnimals; $rabbits++) {
        $chickens = $totalAnimals - $rabbits;
        $legs = $chickens * 2 + $rabbits * 4;
        
        if ($legs == $totalLegs) {
            return [$chickens, $rabbits];
        }
    }
    
    // If no solution found after checking all possibilities
    return "No answer";
}
function main(){
    assert(calculateAnimals(14, 32) === [12, 2]);
    assert(calculateAnimals(10, 16) === "No answer");
    assert(calculateAnimals(5, 12) === [4, 1]);
    assert(calculateAnimals(3, 10) === [1, 2]);
    assert(calculateAnimals(0, 0) === [0, 0]);
}


main();
?>