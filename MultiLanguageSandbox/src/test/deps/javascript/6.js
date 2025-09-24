
/**
 * Find the factorial of N and take the modulo 10007 of the result.
 * 
 * Parameters:
 * - N (number): A non-negative integer representing the input value (N <= 10000).
 * 
 * Returns:
 * - number: The result after taking the modulo 10007 of the factorial of N.
 * 
 * Examples:
 * processRequest(1) returns 1
 */

function processRequest(n) {
    if (n < 0) {
        throw new Error("N must be a non-negative integer");
    }
    
    let result = 1;
    for (let i = 2; i <= n; i++) {
        result = (result * i) % 10007;
    }
    return result;
}
const testProcessRequest = () => {
    console.assert(processRequest(0) === 1, "Expected factorial of 0 modulo 10007 is 1");
    console.assert(processRequest(1) === 1, "Expected factorial of 1 modulo 10007 is 1");
    console.assert(processRequest(2) === 2, "Expected factorial of 2 modulo 10007 is 2");
    console.assert(processRequest(3) === 6, "Expected factorial of 3 modulo 10007 is 6");
    console.assert(processRequest(4) === 24, "Expected factorial of 4 modulo 10007 is 24");
    console.assert(processRequest(10) === 6266, "Expected factorial of 10 modulo 10007 is 6266");
    console.assert(processRequest(10000) === 6991, "Expected factorial of 10000 modulo 10007 is 6991");

    // console.log("All tests passed.");
}

testProcessRequest();