#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Classify the integer x based on the following method:
If x is a single-digit number, x belongs to its own class.
Otherwise, sum the digits of x, get a new x, and iterate until the class is determined.
Parameters:
- x (char*): The integer to be classified.
​    Returns:
​    int: The class to which the integer x belongs.
	>>> classify_integer(24)
	6
*/
int classify_integer(char *x) {
    int num = atoi(x);
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
int main()
{
	assert(classify_integer("24") == 6);
	assert(classify_integer("39") == 3);
	assert(classify_integer("123456789") == 9);
	assert(classify_integer("123456789012345678901234567890") == 9);
	assert(classify_integer("12345") == 6);
    assert(classify_integer("999999999") == 9);
    return 0;
}