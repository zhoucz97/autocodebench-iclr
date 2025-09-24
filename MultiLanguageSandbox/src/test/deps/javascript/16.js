
/**
 * Count the number of valid coin toss sequences with no consecutive heads in n tosses.
 *
 * Parameters:
 * - n (int): The number of coin tosses.
 *
 * Returns:
 *     Number: The count of valid sequences.
 *
 * Examples:
 *     countValidCoinTossSequences(1)
 *     // returns 2
 */

const countValidCoinTossSequences = (n) => {
    if (n === 0) return 0;
    if (n === 1) return 2;
    
    let dp = Array.from({ length: n + 1 }, () => [0, 0]);
    dp[1][0] = 1; // sequences ending with T
    dp[1][1] = 1; // sequences ending with H
    
    for (let i = 2; i <= n; i++) {
        dp[i][0] = dp[i-1][0] + dp[i-1][1]; // can follow H or T
        dp[i][1] = dp[i-1][0]; // can only follow T
    }
    
    return dp[n][0] + dp[n][1];
};
const testCountValidCoinTossSequences = () => {
    console.assert(countValidCoinTossSequences(1) === 2);
    console.assert(countValidCoinTossSequences(2) === 3);
    console.assert(countValidCoinTossSequences(3) === 5);
    console.assert(countValidCoinTossSequences(4) === 8);
    console.assert(countValidCoinTossSequences(5) === 13);
    // Additional tests
    console.assert(countValidCoinTossSequences(40) === 267914296);
    console.assert(countValidCoinTossSequences(39) === 165580141);
    console.assert(countValidCoinTossSequences(38) === 102334155);
    // console.log("All tests passed!");
};

testCountValidCoinTossSequences();