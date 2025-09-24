
/**
 * Calculate the area of a triangle given its base and height.
 * 
 * @param base The base length of the triangle.
 * @param height The height of the triangle.
 * 
 * @returns The calculated area of the triangle, rounded to one decimal place.
 * 
 * Examples:
 * calculateTriangleArea(1, 2) returns 1.0
 * calculateTriangleArea(3, 4) returns 6.0
 */

function calculateTriangleArea(base: number, height: number): number {
    const area = (base * height) / 2;
    return Math.round(area * 10) / 10;
}
const testCalculateTriangleArea = () => {
    // The equivalent of C's assertion with a floating-point comparison in TypeScript.
    const assertCloseTo = (value: number, expected: number, tolerance: number = 1e-6) => {
        console.assert(Math.abs(value - expected) < tolerance);
    };

    assertCloseTo(calculateTriangleArea(1, 2), 1.0);
    assertCloseTo(calculateTriangleArea(3, 4), 6.0);
    assertCloseTo(calculateTriangleArea(5, 8), 20.0);
    assertCloseTo(calculateTriangleArea(7, 3), 10.5);
    assertCloseTo(calculateTriangleArea(10, 10), 50.0);

    // Uncomment to log the outcome to the console.
    // console.log("All tests passed.");
};

testCalculateTriangleArea();