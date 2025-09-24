
#include <assert.h>
#include <stdio.h>
/*
Given a non-negative integer num, repeatedly add all its digits until the result has only one digit.
For example:
    >>> addDigits(38)
    2
    Because 3 + 8 = 11, and 1 + 1 = 2. Since 2 has only one digit, 2 is the result.
*/
int addDigits(int num) {
    if (num == 0) {
        return 0;
    }
    int remainder = num % 9;
    return (remainder == 0) ? 9 : remainder;
}
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