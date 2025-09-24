
/**
 * Calculate the distance between two points A (xa, ya) and B (xb, yb).
 * 
 * @param xa - x-coordinate of point A
 * @param ya - y-coordinate of point A
 * @param xb - x-coordinate of point B
 * @param yb - y-coordinate of point B
 * @returns The distance between points A and B
 * 
 * Example:
 * ```
 * calculateDistance(0, 0, 3, 4) === 5
 * ```
 */

function calculateDistance(xa: number, ya: number, xb: number, yb: number): number {
    const dx = xb - xa;
    const dy = yb - ya;
    return Math.sqrt(dx * dx + dy * dy);
}

// Example usage:
 // Output: 5
const testCalculateDistance = (): void => {
    console.assert(Math.abs(calculateDistance(0, 0, 3, 4) - 5) < 1e-6);
    console.assert(Math.abs(calculateDistance(0, 0, 0, 0) - 0) < 1e-6);
    console.assert(Math.abs(calculateDistance(-1, -1, 2, 2) - 4.242640687) < 1e-6);
    console.assert(Math.abs(calculateDistance(1.5, 3.9, 4.2, -1.1) - 5.68243) < 1e-6);
    // console.log("All test cases passed!");
};

testCalculateDistance();