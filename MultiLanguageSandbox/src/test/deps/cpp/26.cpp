#include <string>
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
/**
 * Classify the integer x based on the following method:
 * If x is a single-digit number, x belongs to its own class.
 * Otherwise, sum the digits of x, get a new x, and iterate until 
 * the class is determined.
 *
 * Parameters:
 * - x (const char*): The integer to be classified in string form.
 *
 * Returns:
 * int: The class to which the integer x belongs.
 *
 * Usage:
 * >>> classify_integer("24")
 * 6
 */
int classify_integer(const char *x) {
    std::string num_str(x);
    int num = 0;
    
    // Convert the string to an integer
    for (char c : num_str) {
        if (c < '0' || c > '9') {
            // Handle non-digit characters if necessary, though problem says it's an integer string
            return -1; // or handle error appropriately
        }
        num = num * 10 + (c - '0');
    }
    
    // Process the number until it's a single digit
    while (num >= 10) {
        int sum = 0;
        int temp = num;
        while (temp > 0) {
            sum += temp % 10;
            temp /= 10;
        }
        num = sum;
    }
    
    return num;
}
int main() {
    assert(classify_integer("24") == 6);
    assert(classify_integer("39") == 3);
    assert(classify_integer("123456789") == 9);
    assert(classify_integer("123456789012345678901234567890") == 9);
    assert(classify_integer("12345") == 6);
    assert(classify_integer("999999999") == 9);
    // std::printf("All tests passed successfully!\n");
    return 0;
}