#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/*
Reads an integer and a character, then returns them as a formatted string separated by a comma.
Parameters:
- integer_value (int): The input integer.
- char_value (char): The input character.
Returns:
- char*: A string containing the integer and character separated by a comma.
>>> process_integer_and_char(234,H)
	234,H
*/
char* process_integer_and_char(int integer_value, char char_value) {
    // Calculate the maximum possible length of the output string
    // Integer can be up to 10 digits (including negative sign), plus comma and null terminator
    int max_length = 12; // 10 digits + 1 for comma + 1 for null terminator
    
    // Allocate memory for the string
    char* result = (char*)malloc(max_length * sizeof(char));
    if (result == NULL) {
        return NULL; // Memory allocation failed
    }
    
    // Format the string
    snprintf(result, max_length, "%d,%c", integer_value, char_value);
    
    return result;
}
int main()
{
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