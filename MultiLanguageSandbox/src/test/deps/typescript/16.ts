
/**
 * Counts the number of valid coin toss sequences with no consecutive heads in n tosses.
 * 
 * @param n - The number of coin tosses.
 * @returns The count of valid sequences.
 * 
 * Examples:
 * countValidCoinTossSequences(1) returns 2
 * countValidCoinTossSequences(2) returns 3
 */

function countValidCoinTossSequences(n: number): number {
    if (n === 0) return 0;
    if (n === 1) return 2;
    
    let dp0 = 1; // sequences ending with T
    let dp1 = 1; // sequences ending with H
    
    for (let i = 2; i <= n; i++) {
        const newDp0 = dp0 + dp1; // sequences ending with T can follow any previous sequence
        const newDp1 = dp0;       // sequences ending with H must follow a sequence ending with T
        dp0 = newDp0;
        dp1 = newDp1;
    }
    
    return dp0 + dp1;
}
const testCountValidCoinTossSequences = (): void => {
    console.assert(countValidCoinTossSequences(1) === 2);
    console.assert(countValidCoinTossSequences(2) === 3);
    console.assert(countValidCoinTossSequences(3) === 5);
    console.assert(countValidCoinTossSequences(4) === 8); // Additional test
    console.assert(countValidCoinTossSequences(5) === 13); // Additional test
    // Feel free to add more tests here
    console.assert(countValidCoinTossSequences(40) === 267914296); // Additional test
    console.assert(countValidCoinTossSequences(39) === 165580141);
    console.assert(countValidCoinTossSequences(38) === 102334155);
    // Uncomment the next line to log a message on successful test pass
    // console.log("All tests passed!");
};

testCountValidCoinTossSequences();