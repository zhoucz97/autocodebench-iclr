#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Calculate the area of a triangle given its three sides.
​    Parameters:
- a (float): Length of side 'a'.
- b (float): Length of side 'b'.
- c (float): Length of side 'c'.
​    Returns:
- str: If the provided sides form a triangle, return the calculated area with 2 decimal places.
Otherwise, return "-1"
    >>> calculate_triangle_area(3,5,4)
    6
*/
char* calculate_triangle_area(double a, double b, double c) {
    // Check if the sides can form a triangle (triangle inequality theorem)
    if (a + b <= c || a + c <= b || b + c <= a) {
        return "-1";
    }
    
    // Calculate semi-perimeter
    double s = (a + b + c) / 2;
    
    // Calculate area using Heron's formula
    double area = sqrt(s * (s - a) * (s - b) * (s - c));
    
    // Convert the area to a string with 2 decimal places
    char* result = (char*)malloc(20 * sizeof(char)); // Allocate enough space
    snprintf(result, 20, "%.2f", area);
    
    return result;
}

// Example usage:
int main()
{
    double area;

    // Triangle with sides 3, 5, 4 should return area 6
    area = calculate_triangle_area(3, 5, 4);
    assert(fabs(area - 6) < 1e-6);

    // Not a triangle with sides 1, 1, 4 should print "Not a triangle."
    area = calculate_triangle_area(1, 1, 4);
    assert(area == -1);

    // Triangle with sides 7, 24, 25 should return area 84
    area = calculate_triangle_area(7, 24, 25);
    assert(fabs(area - 84) < 1e-6);

    // Triangle with sides 10.5, 6.2, 7.3 should return a correct area
    area = calculate_triangle_area(10.5, 6.2, 7.3);
    // printf("%lf\n", area); // Print the area with 2 decimal places
    // For precision issues, we test if the calculated area is close enough to the expected value
    assert(fabs(area - 22.15) < 1e-2);

    
    return 0;
}