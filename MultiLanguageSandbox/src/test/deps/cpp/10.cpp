#include <vector>
#include <cassert>
#include <iostream>
#include <vector>
/**
 * Calculate the sum of even numbers in a given array.
 * 
 * Parameters:
 * - numbers (vector<int>): A vector of integers.
 * - size (int): The size of the array.
 * 
 * Returns:
 * int: The sum of even numbers in the input array.
 * 
 * Examples:
 * >>> calculate_even_sum({1,4,3,2,5}, 5)
 * 6
 */
int calculate_even_sum(const std::vector<int>& numbers) {
    int sum = 0;
    for (int num : numbers) {
        if (num % 2 == 0) {
            sum += num;
        }
    }
    return sum;
}
int main() {
    std::vector<int> sample1 = {1, 4, 3, 2, 5};
    std::vector<int> sample2 = {2, 2, 0, 0};
    std::vector<int> sample3 = {7, 11, 19}; // Additional test with no even numbers
    std::vector<int> sample4 = {12, 14, 16, 18, 20}; // Additional test with all even numbers

    assert(calculate_even_sum(sample1) == 6);
    assert(calculate_even_sum(sample2) == 4);
    assert(calculate_even_sum(sample3) == 0); // Should return 0 because there are no even numbers
    assert(calculate_even_sum(sample4) == 12 + 14 + 16 + 18 + 20);

    // std::cout << "All tests passed successfully.\n";

    return 0;
}