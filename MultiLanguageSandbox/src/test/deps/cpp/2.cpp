#include <cmath>
#include <iomanip>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
/**
 * Calculate the area of a triangle given its three sides.
 *
 * @param a (double): Length of side 'a'.
 * @param b (double): Length of side 'b'.
 * @param c (double): Length of side 'c'.
 *
 * @return (double): If the provided sides form a triangle, return the
 *                   calculated area rounded to 2 decimal places.
 *                   Otherwise, return -1 as an indicator.
 *
 * Example usage:
 *     double area = calculate_triangle_area(3, 5, 4);
 *     assert(fabs(area - 6.00) < 1e-6); // Area should be approximately 6
 */
// For rounding (though we'll use floating-point arithmetic)

double calculate_triangle_area(double a, double b, double c) {
    // Check if the sides can form a triangle (triangle inequality theorem)
    if (a + b <= c || a + c <= b || b + c <= a) {
        return -1;
    }
    
    // Calculate semi-perimeter
    double s = (a + b + c) / 2.0;
    
    // Calculate area using Heron's formula
    double area = sqrt(s * (s - a) * (s - b) * (s - c));
    
    // Round to 2 decimal places (using floating-point arithmetic)
    // Multiply by 100, round, divide by 100
    area = round(area * 100) / 100;
    
    return area;
}
int main() {
    // Test cases for the calculate_triangle_area function
    assert(fabs(calculate_triangle_area(3, 5, 4) - 6.00) < 1e-6); // Expected area is 6
    assert(calculate_triangle_area(1, 1, 4) == -1);  // Not a triangle, should return -1
    assert(fabs(calculate_triangle_area(7, 24, 25) - 84.00) < 1e-6); // Expected area is 84
    assert(fabs(calculate_triangle_area(10.5, 6.2, 7.3) - 22.15) < 1e-2); // Expected area is approx 22.15

    // All tests passed
    
    return 0;
}