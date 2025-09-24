
/**
 * Finds the factorial of N and applies modulo 10007 to the result.
 *
 * Parameters:
 * - N (number): A non-negative integer (N <= 10000).
 *
 * Returns:
 * - number: The factorial of N modulo 10007.
 *
 * Examples:
 * processRequest(1) // returns 1
 * processRequest(2) // returns 2
 * processRequest(10) // returns 6266
 */

function processRequest(n: number): number {
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
    console.assert(processRequest(0) === 1, "processRequest(0) should return 1"); // Added test for boundary condition
    console.assert(processRequest(1) === 1, "processRequest(1) should return 1");
    console.assert(processRequest(2) === 2, "processRequest(2) should return 2");
    console.assert(processRequest(3) === 6, "processRequest(3) should return 6");
    console.assert(processRequest(4) === 24, "processRequest(4) should return 24");
    console.assert(processRequest(10) === 6266, "processRequest(10) should return 6266"); // 10! % 10007 = 3628800 % 10007 = 6266
    console.assert(processRequest(10000) === 6991, "processRequest(10000) should return 6991"); // Added a test for upper boundary condition
};

testProcessRequest();