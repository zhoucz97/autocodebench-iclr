
/**
 * Calculate the total score for a student based on the scores in different subjects.
 * @param score1 - The score for the first subject.
 * @param score2 - The score for the second subject.
 * @param score3 - The score for the third subject.
 * @param score4 - The score for the fourth subject.
 * @param score5 - The score for the fifth subject.
 * @returns The total score obtained by summing up the scores in all subjects.
 * @example
 * calculateTotalScore(100, 100, 100, 100, 100); // returns 500
 */

function calculateTotalScore(score1: number, score2: number, score3: number, score4: number, score5: number): number {
    return score1 + score2 + score3 + score4 + score5;
}

// Example usage:
 // Output: 500
const testCalculateTotalScore = (): void => {
    console.assert(calculateTotalScore(100, 100, 100, 100, 100) === 500);
    console.assert(calculateTotalScore(0, 0, 0, 0, 0) === 0);
    console.assert(calculateTotalScore(20, 30, 40, 10, 50) === 150);
    console.assert(calculateTotalScore(23, 45, 67, 89, 12) === 236);
    console.assert(calculateTotalScore(5, 5, 5, 5, 5) === 25);
};

testCalculateTotalScore();