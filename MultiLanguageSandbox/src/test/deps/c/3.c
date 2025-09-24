#include <math.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Calculate the value of the function for a given input.
    Parameters:
- x (int): Input value for the function.
    Returns:
- double or string: If x is not in the defined domain, returns "Not define".
Otherwise, returns the calculated function value rounded to 5 decimal places.

    Function Definitions:
- For 0 <= x < 10: y = cos(x + 3.0)
- For 10 <= x < 20: y = (cos(x + 7.5))^2
- For 20 <= x < 30: y = (cos(x + 4.0))^4
    >>> calculate_function_value(40)
	Not define
*/
const char* calculate_function_value(int x) {
    static char result[20]; // Buffer to hold the result string
    
    if (x < 0 || x >= 30) {
        return "Not define";
    }
    
    double y;
    if (x >= 0 && x < 10) {
        y = cos(x + 3.0);
    } else if (x >= 10 && x < 20) {
        y = pow(cos(x + 7.5), 2);
    } else if (x >= 20 && x < 30) {
        y = pow(cos(x + 4.0), 4);
    }
    
    // Format the result to 5 decimal places
    snprintf(result, sizeof(result), "%.5f", y);
    return result;
}
int main() {
    const char* result = calculate_function_value(40);
    assert(strcmp(result, "Not define") == 0);
    
    // Additional test cases based on provided function definitions
    assert(strcmp(calculate_function_value(40), "Not define") == 0);
    assert(strcmp(calculate_function_value(5), "-0.14550") == 0);
    assert(strcmp(calculate_function_value(15), "0.76266") == 0);
    assert(strcmp(calculate_function_value(25), "0.31314") == 0);
    assert(strcmp(calculate_function_value(-1), "Not define") == 0);
    return 0;
}