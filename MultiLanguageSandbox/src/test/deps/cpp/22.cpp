#include <cmath>
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
/**
 * Given integers c and d, where a + b = c and a * b = d, this function
 * finds the possible value of a with the condition a <= b. When multiple 
 * pairs (a, b) are possible, it returns the pair with the smallest a.
 * If no valid pair exists, it returns -1.
 *
 * Parameters:
 *  - c (int): The sum of a and b.
 *  - d (int): The product of a and b.
 *
 * Returns:
 *  - int: A possible value of a or -1 if no valid values exist.
 *
 * Examples:
 *  find_integers(7, 11) returns -1
 *  find_integers(5, 6) returns 2
 */
int find_integers(int c, int d) {
    int discriminant = c * c - 4 * d;
    if (discriminant < 0) {
        return -1;
    }
    int sqrt_discriminant = static_cast<int>(std::sqrt(discriminant));
    if (sqrt_discriminant * sqrt_discriminant != discriminant) {
        return -1;
    }
    // Check if (c ± sqrt_discriminant) is divisible by 2
    if ((c - sqrt_discriminant) % 2 != 0 || (c + sqrt_discriminant) % 2 != 0) {
        return -1;
    }
    int a1 = (c - sqrt_discriminant) / 2;
    int a2 = (c + sqrt_discriminant) / 2;
    // Ensure a <= b, so return the smaller one
    return std::min(a1, a2);
}
int main() {
    assert(find_integers(5, 6) == 2);
    assert(find_integers(6, 9) == 3);
    assert(find_integers(7, 12) == 3);
    assert(find_integers(7, 11) == -1);
    assert(find_integers(9, 8) == 1);
    assert(find_integers(10, 25) == 5);
    assert(find_integers(10000, 8765) == -1);

    return 0;
}