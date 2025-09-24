<?php

/**
 * Counts the number of evenly matched pairs of students based on their exam scores.
 * Two students are considered evenly matched if the difference in each subject's score
 * is not greater than 5 and the total score difference is not greater than 10.
 * Input: An integer N representing the number of students followed by N lines of three integers each
 *         representing the scores of each student in Chinese, Mathematics, and English respectively.
 * Output: An integer representing the number of evenly matched pairs of students.
 */

function countEvenlyMatchedPairs($N, $students) {

    $count = 0;
    
    // Iterate through all possible pairs of students
    for ($i = 0; $i < $N; $i++) {
        for ($j = $i + 1; $j < $N; $j++) {
            $student1 = $students[$i];
            $student2 = $students[$j];
            
            // Calculate differences in each subject
            $chineseDiff = abs($student1[0] - $student2[0]);
            $mathDiff = abs($student1[1] - $student2[1]);
            $englishDiff = abs($student1[2] - $student2[2]);
            
            // Calculate total score difference
            $totalDiff = abs(array_sum($student1) - array_sum($student2));
            
            // Check if all conditions are met
            if ($chineseDiff <= 5 && $mathDiff <= 5 && $englishDiff <= 5 && $totalDiff <= 10) {
                $count++;
            }
        }
    }
    
    return $count;
}
function main(){
    assert(countEvenlyMatchedPairs(3, [[90, 90, 90], [85, 95, 90], [80, 100, 91]]) === 2);
    assert(countEvenlyMatchedPairs(3, [[100, 100, 100], [85, 95, 90], [80, 100, 91]]) === 1);
    assert(countEvenlyMatchedPairs(4, [[100, 100, 100],[100, 100, 100],[100, 100, 100],[100, 100, 100]]) === 6);
    
}


main();
?>