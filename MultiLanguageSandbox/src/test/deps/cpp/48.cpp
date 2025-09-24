#include <iostream>
#include <cassert>
#include <cstdio>
/*
Given a non-negative integer num, this function reduces it to a single digit by repeatedly adding all its digits.
For example:
    >>> addDigits(38)
    2
The calculation is because 3 + 8 = 11, and further 1 + 1 = 2. 2 is returned since it is a single-digit number.
*/
int addDigits(int num) {
    while (num >= 10) {
        int sum = 0;
        while (num > 0) {
            sum += num % 10;  // Add the last digit
            num /= 10;        // Remove the last digit
        }
        num = sum;  // Update num to the sum of its digits
    }
    return num;
}

// Example usage:
int main() {
    assert(addDigits(38) == 2);
    assert(addDigits(0) == 0);
    assert(addDigits(9) == 9);
    assert(addDigits(123) == 6);
    assert(addDigits(456) == 6);
    assert(addDigits(9999) == 9);
    assert(addDigits(100) == 1);
    assert(addDigits(1010) == 2);
    assert(addDigits(1234) == 1);
    assert(addDigits(9876) == 3);
    assert(addDigits(199) == 1);
    return 0;
}