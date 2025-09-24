<?php


/**
 * Counts the number of students enrolled in both courses A and B.
 * Given two arrays representing the student IDs of those who have enrolled in courses A and B,
 * this function calculates the total number of students who have enrolled in both courses.
 * 
 * The function accepts two parameters:
 * - $courseA (array): An array of integers representing the IDs of students enrolled in course A.
 * - $courseB (array): An array of integers representing the IDs of students enrolled in course B.
 * 
 * The function returns an integer representing the number of students enrolled in both courses.
 * 
 * Example:
 * If course A has students with IDs [1, 2, 3, 4, 5] and course B has students with IDs [1, 3, 4, 5, 6],
 * the function should return 4 because students with IDs 1, 3, 4, and 5 are enrolled in both courses.
 */

function countCommonStudents(array $courseA, array $courseB): int {

    // Convert both arrays to sets (using array_flip to remove duplicates if any)
    $setA = array_flip($courseA);
    $setB = array_flip($courseB);
    
    // Find the intersection of the two sets
    $commonStudents = array_intersect_key($setA, $setB);
    
    // Return the count of common students
    return count($commonStudents);
}
function main(){
    assert(countCommonStudents([1, 2, 3, 4, 5], [1, 3, 4, 5, 6]) === 4); // Expected: 4
    assert(countCommonStudents([2, 4, 6, 8], [1, 3, 5, 7]) === 0); // Expected: 0, no common students
    assert(countCommonStudents([1, 3, 5, 7, 9], [2, 3, 5, 7]) === 3); // Expected: 3, students with IDs 3, 5, and 7 are common
    
}


main();
?>