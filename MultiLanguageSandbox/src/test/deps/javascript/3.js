
/**
 * Calculate the value of the function for a given input.
 * 
 * Parameters:
 * - x (number): Input value for the function.
 * 
 * Returns:
 * - string: If x is not in the defined domain, returns "Not define".
 * Otherwise, returns the calculated function value as a string rounded to 5 decimal places.
 * 
 * Function Definitions:
 * - For 0 <= x < 10: y = cos(x + 3.0)
 * - For 10 <= x < 20: y = (cos(x + 7.5))^2
 * - For 20 <= x < 30: y = (cos(x + 4.0))^4
 * 
 * Examples:
 * calculateFunctionValue(40); // "Not define"
 */

const calculateFunctionValue = (x) => {
    if (x < 0 || x >= 30) {
        return "Not define";
    } else if (x >= 0 && x < 10) {
        const y = Math.cos(x + 3.0);
        return y.toFixed(5);
    } else if (x >= 10 && x < 20) {
        const y = Math.pow(Math.cos(x + 7.5), 2);
        return y.toFixed(5);
    } else if (x >= 20 && x < 30) {
        const y = Math.pow(Math.cos(x + 4.0), 4);
        return y.toFixed(5);
    }
};
const testCalculateFunctionValue = () => {
    console.assert(calculateFunctionValue(40) === "Not define", "Test case 40 failed");
    console.assert(calculateFunctionValue(5) === "-0.14550", "Test case 5 failed");
    console.assert(calculateFunctionValue(15) === "0.76266", "Test case 15 failed");
    console.assert(calculateFunctionValue(25) === "0.31314", "Test case 25 failed");
    console.assert(calculateFunctionValue(-1) === "Not define", "Test case -1 failed");
}

testCalculateFunctionValue();