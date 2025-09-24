#include <vector>
#include <cmath>
#include <cassert>
#include <cmath>
#include <vector>
/**
 * Determines if it is possible to assemble the wooden squares from n buckets
 * into a single larger square, where each bucket contains squares with a side length of 1.
 * 
 * Input: vector of numbers
 * Example:
 *    >>> Is_Square({9})
 *    true
 * 
 * @param squares Vector containing the count of squares in each bucket.
 * @return True if it is possible to form a larger square, False otherwise.
 */
bool Is_Square(std::vector<int> squares) {
    int total = 0;
    for (int num : squares) {
        total += num;
    }
    int root = std::sqrt(total);
    return root * root == total;
}
int main() {
    assert(Is_Square({9}) == true);
    assert(Is_Square({14, 2}) == true);
    assert(Is_Square({7, 7}) == false);
    assert(Is_Square({1, 2, 3, 4, 5, 6, 7}) == false);
    assert(Is_Square({1, 3, 5, 7, 9, 11}) == true);
    assert(Is_Square({2, 2, 2, 2}) == false);

    // Additional test cases
    assert(Is_Square({4, 5, 6}) == false);
    assert(Is_Square({16, 9, 4, 1}) == false);
    assert(Is_Square({1, 1, 1, 1, 1}) == false);
    assert(Is_Square({25, 25}) == false);
    assert(Is_Square({10, 10, 5}) == true);

    // All tests passed
    return 0;
}