
/**
 * Calculate the area of a triangle given its three sides.
 * 
 * @param {number} a - Length of side 'a'.
 * @param {number} b - Length of side 'b'.
 * @param {number} c - Length of side 'c'.
 * @returns {string|number} If the provided sides form a triangle, return the calculated area with 2 decimal places. Otherwise, return -1.
 * Example:
 *  calculate_triangle_area(3, 5, 4) // returns 6
 */

function calculate_triangle_area(a, b, c) {
    // Check if the sides can form a triangle
    if (a + b > c && a + c > b && b + c > a) {
        // Calculate the semi-perimeter
        const s = (a + b + c) / 2;
        // Calculate the area using Heron's formula
        const area = Math.sqrt(s * (s - a) * (s - b) * (s - c));
        // Return the area rounded to 2 decimal places
        return parseFloat(area.toFixed(2));
    } else {
        // Return -1 if the sides do not form a triangle
        return -1;
    }
}
const testCalculateTriangleArea = () => {
    // Triangle with sides 3, 5, 4 should return area 6
    console.assert(Math.abs(calculate_triangle_area(3, 5, 4) - 6) < 1e-6);

    // Not a triangle with sides 1, 1, 4 should return -1
    console.assert(calculate_triangle_area(1, 1, 4) === -1);

    // Triangle with sides 7, 24, 25 should return area 84
    console.assert(Math.abs(calculate_triangle_area(7, 24, 25) - 84) < 1e-6);

    // Triangle with sides 10.5, 6.2, 7.3 should return a correct area within precision range
    console.assert(Math.abs(calculate_triangle_area(10.5, 6.2, 7.3) - 22.15) < 1e-6);

    // console.log("All tests passed successfully.");
};

testCalculateTriangleArea();