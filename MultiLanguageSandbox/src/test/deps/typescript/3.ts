
/**
 * Calculate the value of the function for a given input.
 * 
 * Parameters:
 * - x (number): Input value for the function.
 * 
 * Returns:
 * - string: If x is not in the defined domain, returns "Not define".
 *   Otherwise, returns the calculated function value rounded to 5 decimal places.
 * 
 * Function Definitions:
 * - For 0 <= x < 10: y = cos(x + 3.0)
 * - For 10 <= x < 20: y = (cos(x + 7.5))^2
 * - For 20 <= x < 30: y = (cos(x + 4.0))^4
 * 
 * Example usage:
 * calculateFunctionValue(40); // Returns "Not define"
 */

function calculateFunctionValue(x: number): string {
    if (x < 0 || x >= 30) {
        return "Not define";
    }

    let y: number;
    if (0 <= x && x < 10) {
        y = Math.cos(x + 3.0);
    } else if (10 <= x && x < 20) {
        y = Math.pow(Math.cos(x + 7.5), 2);
    } else if (20 <= x && x < 30) {
        y = Math.pow(Math.cos(x + 4.0), 4);
    } else {
        // This case shouldn't happen due to the initial check
        return "Not define";
    }

    return y.toFixed(5);
}
function testCalculateFunctionValue(): void {
    console.assert(calculateFunctionValue(40) === "Not define", "Test failed: x = 40 should be 'Not define'");
    console.assert(calculateFunctionValue(5) === "-0.14550", "Test failed: x = 5 should be '-0.14550'");
    console.assert(calculateFunctionValue(15) === "0.76266", "Test failed: x = 15 should be '0.76266'");
    console.assert(calculateFunctionValue(25) === "0.31314", "Test failed: x = 25 should be '0.31314'");
    console.assert(calculateFunctionValue(-1) === "Not define", "Test failed: x = -1 should be 'Not define'");
}

testCalculateFunctionValue();