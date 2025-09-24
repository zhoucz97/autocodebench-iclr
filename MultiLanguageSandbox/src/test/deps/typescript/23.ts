
/**
 * Count the number of acute-angled triangles that can be formed by selecting any 3 edges out of a given set of edges.
 * 
 * @param edges An array of integers representing the lengths of the edges.
 * @returns The count of distinct acute-angled triangles that can be formed.
 * 
 * Examples:
 * ```
 * countAcuteTriangles([1, 1, 1, 1])
 * // returns 4
 * ```
 */

function countAcuteTriangles(edges: number[]): number {
    let count = 0;
    const n = edges.length;
    
    // Iterate through all possible triplets
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            for (let k = j + 1; k < n; k++) {
                const a = edges[i];
                const b = edges[j];
                const c = edges[k];
                
                // Check if they can form a triangle
                if (a + b > c && a + c > b && b + c > a) {
                    // Check if the triangle is acute
                    const aSquared = a * a;
                    const bSquared = b * b;
                    const cSquared = c * c;
                    
                    if (aSquared + bSquared > cSquared && 
                        aSquared + cSquared > bSquared && 
                        bSquared + cSquared > aSquared) {
                        count++;
                    }
                }
            }
        }
    }
    
    return count;
}
(() => {
    // Test function using console.assert for validation
    console.assert(countAcuteTriangles([1, 1, 1, 1]) === 4, "Test 1 Failed");
    console.assert(countAcuteTriangles([1, 2, 3]) === 0, "Test 2 Failed");
    // Additional tests to ensure correctness
    console.assert(countAcuteTriangles([3, 4, 5, 7, 10]) === 0, "Test 3 Failed");
    console.assert(countAcuteTriangles([6, 8, 10, 5, 5, 5]) === 4, "Test 4 Failed");
    // console.log("All tests passed!");
})();