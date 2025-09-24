// No direct imports are needed in JavaScript as we are not using any specific external libraries for this code
/**
 * Calculate the area of a triangle given its base and height.
 * Parameters:
 * - base (number): The base length of the triangle.
 * - height (number): The height of the triangle.
 * Returns:
 * float: The calculated area of the triangle, rounded to one decimal place.
 * Examples:
 * calculate_triangle_area(1,2) returns 1.0
 */

const calculate_triangle_area = (base, height) => {
    const area = (base * height) / 2;
    return Math.round(area * 10) / 10;
};
const testCalculateTriangleArea = () => {
    const epsilon = 1e-6;
    console.assert(Math.abs(calculate_triangle_area(1, 2) - 1.0) < epsilon);
    console.assert(Math.abs(calculate_triangle_area(3, 4) - 6.0) < epsilon);
    console.assert(Math.abs(calculate_triangle_area(5, 8) - 20.0) < epsilon);
    console.assert(Math.abs(calculate_triangle_area(7, 3) - 10.5) < epsilon);
    console.assert(Math.abs(calculate_triangle_area(10, 10) - 50.0) < epsilon);
    
    // console.log("All tests passed.");
};

testCalculateTriangleArea();