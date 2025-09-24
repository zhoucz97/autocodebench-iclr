#include <cmath>
#include <string>
#include <iomanip>
#include <sstream>
#include <cassert>
#include <cmath>
#include <cstring>
#include <iostream>
#include <string>
/**
 * Calculate the value of the function for a given input.
 * Parameters:
 * - x (int): Input value for the function.
 * Returns:
 * - std::string: If x is not in the defined domain, returns "Not define".
 *   Otherwise, returns the calculated function value rounded to 5 decimal places.
 *
 * Function Definitions:
 * - For 0 <= x < 10: y = cos(x + 3.0)
 * - For 10 <= x < 20: y = (cos(x + 7.5))^2
 * - For 20 <= x < 30: y = (cos(x + 4.0))^4
 * Example:
 *   calculate_function_value(40) returns "Not define"
 */
std::string calculate_function_value(int x) {
    double y;
    
    if (x >= 0 && x < 10) {
        y = cos(x + 3.0);
    } else if (x >= 10 && x < 20) {
        y = pow(cos(x + 7.5), 2);
    } else if (x >= 20 && x < 30) {
        y = pow(cos(x + 4.0), 4);
    } else {
        return "Not define";
    }
    
    // Format the result to 5 decimal places
    std::ostringstream oss;
    oss << std::fixed << std::setprecision(5) << y;
    return oss.str();
}
int main() {
    assert(calculate_function_value(40) == "Not define");

    // Additional test cases based on provided function definitions
    assert(calculate_function_value(5) == "-0.14550");
    assert(calculate_function_value(15) == "0.76266");
    assert(calculate_function_value(25) == "0.31314");
    assert(calculate_function_value(-1) == "Not define");

    // std::cout << "All tests passed successfully." << std::endl;

    return 0;
}