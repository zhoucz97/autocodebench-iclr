
#include <assert.h> // Used for testing the function with assertions
#include <math.h> // Not needed for this function
#include <stdbool.h> // Not needed for this function
#include <stdio.h> // Needed for input/output functions
#include <string.h> // Not needed for this function
/*
Evaluate the grade for an input integer.
    Parameters:
- score (int): The integer score to be evaluated.
    Returns:
​​    char: The grade corresponding to the input score.
​​         If the score is between 90 and 100 (inclusive), returns 'A'.
​​         Otherwise, returns 'B'.
>>> evaluate_integer_grade(90)
A
*/
char evaluate_integer_grade(int score) {
    if (score >= 90 && score <= 100) {
        return 'A';
    } else {
        return 'B';
    }
}
int main() // Updated test function to reflect the correct usage of the evaluate_integer_grade function
{
    assert(evaluate_integer_grade(90) == 'A'); // Corrected the assertion to compare against character literals
    assert(evaluate_integer_grade(89) == 'B'); // Additional test for a score less than 90
    assert(evaluate_integer_grade(95) == 'A'); // Additional test within the 'A' range
    assert(evaluate_integer_grade(100) == 'A'); // Additional test for the boundary condition
    assert(evaluate_integer_grade(101) == 'B'); // Additional test for a score greater than 100

    // Print "All tests passed" if all assertions hold true (optional)
    // printf("All tests passed\n");

    return 0;
}