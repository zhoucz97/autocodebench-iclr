
#include <cassert>
#include <cstdio>
/**
 * Determines if two closed intervals intersect.
 *
 * @param a The lower bound of the first closed interval [a,b].
 * @param b The upper bound of the first closed interval [a,b].
 * @param c The lower bound of the second closed interval [c,d].
 * @param d The upper bound of the second closed interval [c,d].
 *
 * @return 1 if the intervals intersect, 0 otherwise.
 *
 * Examples:
 * are_intervals_intersecting(1, 1, 1, 2) returns 1
 * are_intervals_intersecting(3, 5, 2, 6) returns 1
 * are_intervals_intersecting(3, 5, 4, 7) returns 1
 * are_intervals_intersecting(3, 5, 6, 7) returns 0
 */
int are_intervals_intersecting(int a, int b, int c, int d) {
    // Check if the intervals intersect
    if (b < c || d < a) {
        return 0; // No intersection
    } else {
        return 1; // Intersection exists
    }
}
int main() {
    assert(are_intervals_intersecting(1, 1, 1, 2) == 1);
    assert(are_intervals_intersecting(3, 5, 2, 6) == 1);
    assert(are_intervals_intersecting(3, 5, 4, 7) == 1);
    assert(are_intervals_intersecting(3, 5, 6, 7) == 0);
    // Additional test cases
    assert(are_intervals_intersecting(0, 0, 0, 0) == 1);
    assert(are_intervals_intersecting(1, 3, 2, 4) == 1);
    assert(are_intervals_intersecting(1, 3, 4, 6) == 0);
    assert(are_intervals_intersecting(10, 20, 20, 30) == 1);
    assert(are_intervals_intersecting(10, 20, 21, 30) == 0);
    
    // printf("All tests passed.\n");
    return 0;
}