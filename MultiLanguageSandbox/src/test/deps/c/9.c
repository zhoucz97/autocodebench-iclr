#include <stdarg.h>
#include <assert.h>
#include <stdarg.h>

/*
Count the number of odd integers in a given list of numbers.
    Parameters:
- count (int): The count of numbers to evaluate.
- ... (int): A sequence of integers.
    Returns:
    int: The count of odd numbers in the input list.
    >>> count_odd_numbers(5, 1, 4, 3, 2, 5)
    3
*/
int count_odd_numbers(int count, ...) {
    va_list args;
    va_start(args, count);
    
    int odd_count = 0;
    for (int i = 0; i < count; i++) {
        int num = va_arg(args, int);
        if (num % 2 != 0) {
            odd_count++;
        }
    }
    
    va_end(args);
    return odd_count;
}
int main() {
    assert(count_odd_numbers(5, 1, 4, 3, 2, 5) == 3);
    assert(count_odd_numbers(4, 2, 2, 0, 0) == 0);
    assert(count_odd_numbers(6, 7, 7, 8, 1, 9, 10) == 4); // Additional Test Sample
    // printf("All tests passed!\n");
    return 0;
}