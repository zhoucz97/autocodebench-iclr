
/**
 * Given a 1 by n pixel image, each pixel has a color represented by an integer. You can change all
 * connected pixels of the same color to a chosen color, where two pixels are connected if they are
 * adjacent and have the same color. The goal is to find the minimum number of operations required
 * to make all pixels the same color.
 * Note: For each color, there are at most 20 pixels of that color.
 *
 * Examples:
 *    minOperations(5, [1, 2, 3, 2, 1]) returns 2
 */

function minOperations(n: number, pixels: number[]): number {
    const uniqueColors = new Set(pixels);
    let minOps = Infinity;

    for (const color of uniqueColors) {
        let operations = 0;
        let i = 0;
        while (i < n) {
            const currentColor = pixels[i];
            if (currentColor !== color) {
                operations++;
            }
            // Skip all connected pixels of the same color
            while (i < n && pixels[i] === currentColor) {
                i++;
            }
        }
        if (operations < minOps) {
            minOps = operations;
        }
    }

    return minOps;
}
(() => {
    console.assert(minOperations(5, [1, 2, 3, 2, 1]) === 2);
    console.assert(minOperations(4, [1, 1, 2, 2]) === 1);
    console.assert(minOperations(5, [1, 2, 1, 4, 2]) === 3);
    console.assert(minOperations(5, [5, 5, 5, 5, 5]) === 0);
    console.assert(minOperations(6, [1, 1, 1, 2, 2, 2]) === 1);
    console.assert(minOperations(7, [1, 3, 3, 3, 2, 2, 2]) === 2);
    console.assert(minOperations(8, [4, 4, 4, 4, 3, 3, 3, 3]) === 1);
    console.assert(minOperations(9, [1, 2, 3, 4, 5, 6, 7, 8, 9]) === 8);
    console.assert(minOperations(10, [1, 2, 1, 2, 1, 2, 1, 2, 1, 2]) === 5);
    console.assert(minOperations(3, [3, 3, 3]) === 0);
    console.assert(minOperations(4, [2, 1, 1, 2]) === 1);
})();