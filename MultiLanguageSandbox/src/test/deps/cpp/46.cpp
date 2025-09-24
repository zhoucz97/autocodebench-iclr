#include <iostream>
#include <cassert>
#include <iostream>
/**
 * @brief Computes the sum of all numbers from 1 to n that are multiples of either 3 or 5.
 *
 * Counts each number only once even if it is a multiple of both 3 and 5.
 * For example:
 *     @code
 *     std::cout << sumOfMultiples(10); // Outputs: 33 (3 + 5 + 6 + 9 + 10)
 *     @endcode
 *
 * @param n The upper bound of the range to check for multiples.
 * @return The sum of the multiples of either 3 or 5 within the range.
 */
int sumOfMultiples(int n) {
    int sum = 0;
    for (int i = 1; i <= n; ++i) {
        if (i % 3 == 0 || i % 5 == 0) {
            sum += i;
        }
    }
    return sum;
}
int main()
{
    assert(sumOfMultiples(10) == 33);
    assert(sumOfMultiples(15) == 60);
    assert(sumOfMultiples(20) == 98);
    assert(sumOfMultiples(5) == 8);
    assert(sumOfMultiples(3) == 3);
    assert(sumOfMultiples(6) == 14);
    assert(sumOfMultiples(9) == 23);
    assert(sumOfMultiples(12) == 45);
    assert(sumOfMultiples(17) == 60);
    assert(sumOfMultiples(21) == 119);
    assert(sumOfMultiples(25) == 168);

    return 0;
}