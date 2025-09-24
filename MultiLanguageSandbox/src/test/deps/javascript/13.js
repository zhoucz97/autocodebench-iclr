
/**
 * Calculate the total score for a student based on the scores in different subjects.
 * @param {number} score1 - The score in the first subject.
 * @param {number} score2 - The score in the second subject.
 * @param {number} score3 - The score in the third subject.
 * @param {number} score4 - The score in the fourth subject.
 * @param {number} score5 - The score in the fifth subject.
 * @returns {number} The total score obtained by summing up the scores in all subjects.
 * @example
 * calculateTotalScore(100, 100, 100, 100, 100); // returns 500
 */

function calculateTotalScore(score1, score2, score3, score4, score5) {
    return score1 + score2 + score3 + score4 + score5;
}
function testCalculateTotalScore() {
    console.assert(calculateTotalScore(100, 100, 100, 100, 100) === 500, 'Test failed: Expected 500 for all 100 scores');
    console.assert(calculateTotalScore(0, 0, 0, 0, 0) === 0, 'Test failed: Expected 0 for all 0 scores');
    console.assert(calculateTotalScore(20, 30, 40, 10, 50) === 150, 'Test failed: Expected 150 for mixed scores');
    console.assert(calculateTotalScore(23, 45, 67, 89, 12) === 236, 'Test failed: Expected 236 for mixed scores');
    console.assert(calculateTotalScore(5, 5, 5, 5, 5) === 25, 'Test failed: Expected 25 for all 5 scores');
}

testCalculateTotalScore(); // Running the test function to validate our code