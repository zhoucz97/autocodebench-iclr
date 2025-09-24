#include <cstring>
#include <sstream>
#include <bits/stdc++.h>
/**
 * Reads an integer and a character, then returns them as a formatted string separated by a comma.
 * 
 * Parameters:
 * - integer_value (int): The input integer.
 * - char_value (char): The input character.
 * 
 * Returns:
 * - char*: A string containing the integer and character separated by a comma.
 * 
 * Example:
 * process_integer_and_char(234, 'H') returns "234,H"
 */
char* process_integer_and_char(int integer_value, char char_value) {
    // Create a stringstream to format the output
    std::ostringstream oss;
    oss << integer_value << "," << char_value;
    
    // Get the formatted string
    std::string result = oss.str();
    
    // Allocate memory for the C-style string
    char* output = new char[result.length() + 1];
    
    // Copy the string to the allocated memory
    strcpy(output, result.c_str());
    
    return output;
}
int main() {
    // Test 1
    char* output1 = process_integer_and_char(234, 'H');
    assert(strcmp(output1, "234,H") == 0);
    free(output1); // Don't forget to free the allocated memory

    // Additional tests can be added here with different input values
    // Test 2
    char* output2 = process_integer_and_char(123, 'A');
    assert(strcmp(output2, "123,A") == 0);
    free(output2);

    // Test 3
    char* output3 = process_integer_and_char(0, 'Z');
    assert(strcmp(output3, "0,Z") == 0);
    free(output3);

    return 0;
}