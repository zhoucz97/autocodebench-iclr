
#include <assert.h>
#include <stdio.h>
/*
Calculate the sum of even numbers in a given list.
    Parameters:
- numbers (list): A list of integers.
- size (int): The size of the list.
    Returns:
    int: The sum of even numbers in the input list.
    >>> calculate_even_sum([1,4,3,2,5], 5)
    6
*/
int calculate_even_sum(int *numbers, int size) {
    int sum = 0;
    for (int i = 0; i < size; i++) {
        if (numbers[i] % 2 == 0) {
            sum += numbers[i];
        }
    }
    return sum;
}
int main() {
    int sample1[] = {1, 4, 3, 2, 5};
    int sample2[] = {2, 2, 0, 0};
    int sample3[] = {7, 11, 19}; // Additional test with no even numbers
    int sample4[] = {12, 14, 16, 18, 20}; // Additional test with all even numbers

    assert(calculate_even_sum(sample1, 5) == 6);
    assert(calculate_even_sum(sample2, 4) == 4);
    assert(calculate_even_sum(sample3, 3) == 0); // Should return 0 because there are no even numbers
    assert(calculate_even_sum(sample4, 5) == 12 + 14 + 16 + 18 + 20);

    

    return 0;
}