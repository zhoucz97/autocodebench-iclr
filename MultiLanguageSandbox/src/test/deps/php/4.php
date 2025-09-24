<?php

/**
 * Converts a numerical score to its corresponding letter grade.
 * The grading system is as follows:
 * - A score between 90 and 100 (inclusive) is graded as 'A'.
 * - A score between 80 and 89 (inclusive) is graded as 'B'.
 * - A score between 70 and 79 (inclusive) is graded as 'C'.
 * - A score between 60 and 69 (inclusive) is graded as 'D'.
 * - A score below 60 is graded as 'E'.
 * If the score is outside the range of 0 to 100, an error message is returned.
 * Input: A numeric score.
 * Output: Returns the corresponding letter grade or an error message if the score is out of range.
 */
 
function convertScoreToGrade($score) {

    // Check if the score is within the valid range (0 to 100)
    if ($score < 0 || $score > 100) {
        return "Error: Score must be between 0 and 100.";
    }
    
    // Determine the letter grade based on the score
    if ($score >= 90) {
        return 'A';
    } elseif ($score >= 80) {
        return 'B';
    } elseif ($score >= 70) {
        return 'C';
    } elseif ($score >= 60) {
        return 'D';
    } else {
        return 'E';
    }
}
function main(){
    assert(convertScoreToGrade(95) === 'A');
    assert(convertScoreToGrade(85) === 'B');
    assert(convertScoreToGrade(75) === 'C');
    assert(convertScoreToGrade(65) === 'D');
    assert(convertScoreToGrade(55) === 'E');
    assert(convertScoreToGrade(-1) === 'Score is error!');
    assert(convertScoreToGrade(101) === 'Score is error!');
    
}

main();
?>