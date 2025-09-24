#include <math.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Calculate the distance between two points A (xa, ya) and B (xb, yb).
    Parameters:
- xa (double): x-coordinate of point A.
- ya (double): y-coordinate of point A.
- xb (double): x-coordinate of point B.
- yb (double): y-coordinate of point B.
    
    Returns:
    double: The distance between points A and B.
    >>> calculate_distance(0,0,3,4)
    5
*/
double calculate_distance(double xa, double ya, double xb, double yb) {
    double dx = xb - xa;
    double dy = yb - ya;
    return sqrt(dx * dx + dy * dy);
}
int main() {
    assert(fabs(calculate_distance(0,0,3,4) - 5) < 1e-6);
    assert(fabs(calculate_distance(0,0,0,0) - 0) < 1e-6);
    assert(fabs(calculate_distance(-1,-1,2,2) - 4.242640687) < 1e-6);
    assert(fabs(calculate_distance(1.5,3.9,4.2,-1.1) - 5.68243) < 1e-6);
    // printf("All test cases passed!\n");
    return 0;
}