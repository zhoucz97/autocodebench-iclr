
/**
 * Count the number of acute-angled triangles that can be formed by selecting any 3 edges out of a given set of edges.
 * 
 * @param {number} edges_count - The number of edges.
 * @param {...number} edges - The lengths of the edges.
 * @returns {number} The count of distinct acute-angled triangles that can be formed.
 * 
 * Examples:
 * count_acute_triangles(4, 1, 1, 1, 1) // returns 4
 */

function count_acute_triangles(edges_count, ...edges) {
    let count = 0;
    // Generate all possible combinations of 3 distinct edges
    for (let i = 0; i < edges.length; i++) {
        for (let j = i + 1; j < edges.length; j++) {
            for (let k = j + 1; k < edges.length; k++) {
                const a = edges[i];
                const b = edges[j];
                const c = edges[k];
                // Check triangle inequality
                if (a + b > c && a + c > b && b + c > a) {
                    // Find the largest side
                    const maxSide = Math.max(a, b, c);
                    let sumOfSquares = 0;
                    if (maxSide === a) {
                        sumOfSquares = b * b + c * c;
                    } else if (maxSide === b) {
                        sumOfSquares = a * a + c * c;
                    } else {
                        sumOfSquares = a * a + b * b;
                    }
                    // Check acute condition
                    if (sumOfSquares > maxSide * maxSide) {
                        count++;
                    }
                }
            }
        }
    }
    return count;
}
(function testAcuteTriangles() {
    console.assert(count_acute_triangles(4, 1, 1, 1, 1) === 4, "Test case 1 failed");
    console.assert(count_acute_triangles(3, 1, 2, 3) === 0, "Test case 2 failed");
    // Additional tests to ensure correctness
    console.assert(count_acute_triangles(5, 3, 4, 5, 7, 10) === 0, "Test case 3 failed");
    console.assert(count_acute_triangles(6, 6, 8, 10, 5, 5, 5) === 4, "Test case 4 failed");
})();