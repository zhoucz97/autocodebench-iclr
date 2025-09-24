#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Calculate the area of a triangle given its base and height.
​    Parameters:
- base (int): The base length of the triangle.
- height (int): The height of the triangle.
​    Returns:
​    float: The calculated area of the triangle, rounded to one decimal place.
	>>> calculate_triangle_area(1,2)
	1.0
*/
float calculate_triangle_area(int base, int height) {
    float area = 0.5 * base * height;
    // Round to one decimal place
    area = roundf(area * 10) / 10;
    return area;
}
int main()
{
    // Adding more comprehensive test cases and correcting the expected values
    assert(fabs(calculate_triangle_area(1, 2) - 1.0) < 1e-6); // Should assert true with close approximate comparison
    assert(fabs(calculate_triangle_area(3, 4) - 6.0) < 1e-6); // Should assert true with close approximate comparison
    assert(fabs(calculate_triangle_area(5, 8) - 20.0) < 1e-6); // Additional test case
    assert(fabs(calculate_triangle_area(7, 3) - 10.5) < 1e-6); // Additional test case
    assert(fabs(calculate_triangle_area(10, 10) - 50.0) < 1e-6); // Additional test case

    // printf("All tests passed.\n");
    return 0;
}