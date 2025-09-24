<?php

/**
 * Determines the position of a new student in a choir formation.
 * The choir is arranged with the shortest members on the sides and the tallest in the center.
 * The new student may stand on the left or right side, or, if they are the tallest, to the right of the current tallest student.
 * If the new student is not the tallest, their position is based on the teacher's preference: left or right side.
 *
 * Input:
 * - $newStudentHeight (int): The height of the new student.
 * - $heights (array): An array of integers representing the heights of the current choir members from left to right.
 * - $teacherPreference (int): The teacher's preference for the new student's position (1 for left, 2 for right).
 *
 * Output:
 * - Returns an integer representing the position (1-indexed) where the new student will stand.
 *
 * Examples:
 * - determineChoirPosition(20, [1, 3, 8, 10, 14, 40, 22, 16, 8], 2) should return 8.
 * - determineChoirPosition(15, [5, 10, 20, 10, 5], 3) should return 3.
 */

function determineChoirPosition(int $newStudentHeight, array $heights, int $teacherPreference): int {

    $n = count($heights);
    if ($n == 0) {
        return 1;
    }
    
    $maxHeight = max($heights);
    $maxIndex = array_search($maxHeight, $heights);
    
    if ($newStudentHeight > $maxHeight) {
        // Place to the right of the current tallest
        return $maxIndex + 2;
    } else {
        $leftSide = array_slice($heights, 0, ($n + 1) / 2);
        $rightSide = array_slice($heights, ($n + 1) / 2);
        
        if ($teacherPreference == 1) {
            // Left side: find the first position where the next element is >= newStudentHeight
            $pos = 0;
            while ($pos < count($leftSide) && $leftSide[$pos] < $newStudentHeight) {
                $pos++;
            }
            return $pos + 1;
        } else {
            // Right side: find the first position where the next element is <= newStudentHeight
            $pos = 0;
            while ($pos < count($rightSide) && $rightSide[$pos] > $newStudentHeight) {
                $pos++;
            }
            return ($n + 1) / 2 + $pos + 1;
        }
    }
}
function main(){
    
    assert(determineChoirPosition(20, [1, 3, 8, 10, 14, 40, 22, 16, 8], 2) === 8);
    assert(determineChoirPosition(15, [5, 10, 20, 10, 5], 1) === 3);
    assert(determineChoirPosition(12, [10, 15, 20, 15], 2) === 5);
    assert(determineChoirPosition(18, [6, 9, 12, 15, 17, 21], 1) === 6);
    assert(determineChoirPosition(18, [6, 9, 12, 15], 1) === 5);
    assert(determineChoirPosition(10, [5, 8, 12], 2) === 4);   
}


main();
?>