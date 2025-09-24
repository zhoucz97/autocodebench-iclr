
/**
 * Evaluate the grade for an input integer.
 * @param score - The integer score to be evaluated.
 * @returns The grade corresponding to the input score.
 *          If the score is between 90 and 100 (inclusive), returns 'A'.
 *          Otherwise, returns 'B'.
 * 
 * Examples:
 * evaluateIntegerGrade(90) // returns 'A'
 *
 */

function evaluateIntegerGrade(score: number): string {
    if (score >= 90 && score <= 100) {
        return 'A';
    } else {
        return 'B';
    }
}
const testEvaluateIntegerGrade = (): void => {
    console.assert(evaluateIntegerGrade(90) === 'A', "Expected 'A' for score 90");
    console.assert(evaluateIntegerGrade(89) === 'B', "Expected 'B' for score 89");
    console.assert(evaluateIntegerGrade(95) === 'A', "Expected 'A' for score 95");
    console.assert(evaluateIntegerGrade(100) === 'A', "Expected 'A' for score 100");
    console.assert(evaluateIntegerGrade(101) === 'B', "Expected 'B' for score 101");
    
    // console.log("All tests passed");
};

testEvaluateIntegerGrade();