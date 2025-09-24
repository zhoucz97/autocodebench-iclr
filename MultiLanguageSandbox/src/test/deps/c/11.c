
#include <assert.h>
#include <stdio.h>
/*
Determine if two closed intervals intersect.
​    Args:
​    a, b: Representing the first closed interval [a, b] where 0 <= a <= b <= 1000.
​    c, d: Representing the second closed interval [c, d] where 0 <= c <= d <= 1000.
​    Returns:
​    int: 1 if the intervals intersect, 0 otherwise.
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
    
    return 0;
}