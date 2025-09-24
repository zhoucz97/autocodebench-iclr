<?php


/**
 * Finds the positions of the k-th largest number in an array.
 * This function takes an array of integers and an integer k, 
 * and returns the positions of the k-th largest number in the array. 
 * If there are duplicates of the k-th largest number, it returns all positions.
 * If k is greater than the array's length or the array is empty, it returns an empty array.
 *
 * Example:
 * Input: numbers = [67, 43, 90, 78, 32], k = 1
 * Output: [3]
 * 
 * Input: numbers = [1, 2, 3, 4, 5], k = 3
 * Output: [3]
 */
 function findKthLargestPositions(array $numbers, int $k): array {

    // Check if k is valid (greater than 0 and not larger than array length)
    if ($k <= 0 || empty($numbers) || $k > count($numbers)) {
        return [];
    }
    
    // Create an array of value-position pairs
    $valuePositionPairs = [];
    foreach ($numbers as $index => $number) {
        $valuePositionPairs[] = ['value' => $number, 'position' => $index];
    }
    
    // Sort the pairs in descending order based on value
    usort($valuePositionPairs, function($a, $b) {
        return $b['value'] - $a['value'];
    });
    
    // Get the k-th largest value (0-based index would be k-1)
    $kthLargestValue = $valuePositionPairs[$k - 1]['value'];
    
    // Collect all positions where the value equals the k-th largest value
    $positions = [];
    foreach ($valuePositionPairs as $pair) {
        if ($pair['value'] == $kthLargestValue) {
            $positions[] = $pair['position'];
        } else {
            // Since the array is sorted, we can break early once we find a smaller value
            break;
        }
    }
    
    return $positions;
}
function main(){
    // Test case 1: k-th largest is unique
    assert(findKthLargestPositions([67, 43, 90, 78, 32], 1) === [3]);

    // Test case 2: k-th largest has duplicates
    assert(findKthLargestPositions([20, 50, 20, 40], 3) === [1, 3]);

    // Test case 3: k is larger than the array length
    assert(findKthLargestPositions([10, 20, 30], 4) === []);

    // Test case 4: Array is empty
    assert(findKthLargestPositions([], 1) === []);

    // Test case 5: k-th largest in the middle
    assert(findKthLargestPositions([1, 2, 3, 4, 5], 3) === [3]);
}


main();
?>