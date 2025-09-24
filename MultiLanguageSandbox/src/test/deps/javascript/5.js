
/**
 * Calculate the distance between two points A (xa, ya) and B (xb, yb).
 *
 * Parameters:
 * - xa (number): x-coordinate of point A.
 * - ya (number): y-coordinate of point A.
 * - xb (number): x-coordinate of point B.
 * - yb (number): y-coordinate of point B.
 *
 * Returns:
 * number: The distance between points A and B.
 * Example usage:
 * console.log(calculateDistance(0,0,3,4)); // Expected output: 5
 */

function calculateDistance(xa, ya, xb, yb) {
    const dx = xb - xa;
    const dy = yb - ya;
    return Math.sqrt(dx * dx + dy * dy);
}

// Example usage:
 // Expected output: 5
const testCalculateDistance = () => {
    console.assert(Math.abs(calculateDistance(0, 0, 3, 4) - 5) < 1e-6);
    console.assert(Math.abs(calculateDistance(0, 0, 0, 0) - 0) < 1e-6);
    console.assert(Math.abs(calculateDistance(-1, -1, 2, 2) - 4.242640687) < 1e-6);
    console.assert(Math.abs(calculateDistance(1.5, 3.9, 4.2, -1.1) - 5.68243) < 1e-6);
    // console.log("All test cases passed!");
}

testCalculateDistance();