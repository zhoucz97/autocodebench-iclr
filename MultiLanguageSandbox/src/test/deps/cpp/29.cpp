
#include <cassert> // Used for making assertions in test cases
/**
 * @brief Evaluate the grade for an input integer.
 * 
 * @param score The integer score to be evaluated.
 * @return char The grade corresponding to the input score.
 *              If the score is between 90 and 100 (inclusive), returns 'A'.
 *              Otherwise, returns 'B'.
 * 
 * Examples:
 *     evaluate_integer_grade(90) -> 'A'
 *     evaluate_integer_grade(89) -> 'B'
 */
char evaluate_integer_grade(int score) {
    if (score >= 90 && score <= 100) {
        return 'A';
    } else {
        return 'B';
    }
}
int main() {
    assert(evaluate_integer_grade(90) == 'A'); // Test for lower bound of 'A' grade
    assert(evaluate_integer_grade(89) == 'B'); // Test for score just below 'A' grade
    assert(evaluate_integer_grade(95) == 'A'); // Test for a score well within the 'A' range
    assert(evaluate_integer_grade(100) == 'A'); // Test for upper boundary of 'A' grade
    assert(evaluate_integer_grade(101) == 'B'); // Test for score above 'A' grade range

    // Print "All tests passed" if all assertions hold true (optional)
    // std::cout << "All tests passed\n";

    return 0;
}