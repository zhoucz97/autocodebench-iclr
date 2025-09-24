<?php


/**
 * Identifies the missing (broken) and duplicate IDs from a given set of ID groups.
 * Each ID group represents a collection of ticket IDs which are supposed to be consecutive but contain a mistake:
 * one ID is missing, and one ID is duplicated. This function calculates and returns the missing and duplicated IDs.
 *
 * Note: The missing ID will not be at the start or end of the overall ID range.
 *
 * Input: An array of arrays, where each sub-array contains integers representing ticket IDs.
 * Output: An array with two integers, the first being the missing (broken) ID and the second the duplicate ID.
 *
 * Example:
 * Input: [[5, 6, 8, 11, 9], [10, 12, 9]]
 * Output: [7, 9] // 7 is missing, and 9 is duplicated.
 */
function findMissingAndDuplicateIDs(array $idGroups): array {

    // Flatten the array of arrays into a single array
    $allIds = array_merge(...$idGroups);
    
    // Find the min and max in the flattened array to determine the expected range
    $min = min($allIds);
    $max = max($allIds);
    
    // Create an array to count occurrences of each ID
    $count = array_count_values($allIds);
    
    $missing = null;
    $duplicate = null;
    
    // Iterate through the expected range to find the missing and duplicate IDs
    for ($i = $min; $i <= $max; $i++) {
        if (!isset($count[$i])) {
            $missing = $i;
        } elseif ($count[$i] > 1) {
            $duplicate = $i;
        }
    }
    
    return [$missing, $duplicate];
}
function main(){

    // Test case based on the example provided
    assert(findMissingAndDuplicateIDs([[5, 6, 8, 11, 9], [10, 12, 9]]) === [7, 9]);
    
    // Additional test cases
    assert(findMissingAndDuplicateIDs([[1, 2, 4], [3, 5, 5, 7]]) === [6, 5]);
    assert(findMissingAndDuplicateIDs([[10, 11, 12, 14, 17], [13, 15, 12]]) === [16, 12]);
    
}


main();
?>