
/**
 * Evaluate the grade for an input integer.
 * 
 * Parameters:
 * - score (int): The integer score to be evaluated.
 * 
 * Returns:
 * ​​    char: The grade corresponding to the input score.
 * ​​         If the score is between 90 and 100 (inclusive), returns 'A'.
 * ​​         Otherwise, returns 'B'.
 * 
 * @example
 * evaluate_integer_grade(90) // returns 'A'
 */

function evaluate_integer_grade(score) {
    if (score >= 90 && score <= 100) {
        return 'A';
    } else {
        return 'B';
    }
}
function testEvaluateIntegerGrade() {
    console.assert(evaluate_integer_grade(90) === 'A', 'Score of 90 should yield an A');
    console.assert(evaluate_integer_grade(89) === 'B', 'Score of 89 should yield a B');
    console.assert(evaluate_integer_grade(95) === 'A', 'Score of 95 should yield an A');
    console.assert(evaluate_integer_grade(100) === 'A', 'Score of 100 should yield an A');
    console.assert(evaluate_integer_grade(101) === 'B', 'Score of 101 should yield a B');

    // console.log("All tests passed!");
}

testEvaluateIntegerGrade();