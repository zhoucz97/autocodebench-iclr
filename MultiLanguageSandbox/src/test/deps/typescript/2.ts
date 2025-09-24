
/**
 * Calculate the area of a triangle given its three sides.
 * 
 * @param a - Length of side 'a'.
 * @param b - Length of side 'b'.
 * @param c - Length of side 'c'.
 * @returns If the provided sides form a triangle, return the calculated area with 2 decimal places.
 *          Otherwise, return "-1".
 * 
 * @example
 * calculateTriangleArea(3, 5, 4)  // 6
 */

function calculateTriangleArea(a: number, b: number, c: number): number {
    // Check if the sides can form a triangle
    if (a + b <= c || a + c <= b || b + c <= a) {
        return -1;
    }
    
    // Calculate the semi-perimeter
    const s = (a + b + c) / 2;
    
    // Calculate the area using Heron's formula
    const area = Math.sqrt(s * (s - a) * (s - b) * (s - c));
    
    // Round to 2 decimal places
    return Math.round(area * 100) / 100;
}
(() => {
    let area: number;

    // Triangle with sides 3, 5, 4 should return area 6
    area = calculateTriangleArea(3, 5, 4);
    console.assert(Math.abs(area - 6) < 1e-6);

    // Not a triangle with sides 1, 1, 4 should return "-1"
    area = calculateTriangleArea(1, 1, 4);
    console.assert(area === -1);

    // Triangle with sides 7, 24, 25 should return area 84
    area = calculateTriangleArea(7, 24, 25);
    console.assert(Math.abs(area - 84) < 1e-6);

    // Triangle with sides 10.5, 6.2, 7.3 should return a correct area
    area = calculateTriangleArea(10.5, 6.2, 7.3);
    // For precision issues, we test if the calculated area is close enough to the expected value
    console.assert(Math.abs(area - 22.15) < 1e-2);

    // console.log("All tests passed successfully.");
})();