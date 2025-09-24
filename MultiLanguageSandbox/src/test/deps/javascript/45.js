
/**
 * Given a 1 by n pixel image, each pixel has a color represented by an integer. You can change all 
 * connected pixels of the same color to a chosen color, where two pixels are connected if they are 
 * adjacent and have the same color. The goal is to find the minimum number of operations required 
 * to make all pixels the same color.
 * Note: For each color, there are at most 20 pixels of that color.
 * 
 * Examples:
 *    extraNumber(5, [1, 2, 3, 2, 1]) returns 2
 */

function minOperations(n, pixels) {
    if (n === 0) return 0;
    
    // Count the frequency of each color
    const colorCount = {};
    for (const color of pixels) {
        colorCount[color] = (colorCount[color] || 0) + 1;
    }
    
    // Find the color with the maximum frequency
    let maxColor = null;
    let maxCount = 0;
    for (const color in colorCount) {
        if (colorCount[color] > maxCount) {
            maxCount = colorCount[color];
            maxColor = color;
        }
    }
    
    // Check if all pixels are already the same color
    if (maxCount === n) {
        return 0;
    }
    
    // Now, we need to find the minimal operations to turn all pixels into maxColor
    // The idea is to find the number of segments of maxColor and other colors
    // The minimal operations is the number of other colors' segments plus (segments of maxColor - 1)
    let segments = 0;
    let currentColor = null;
    
    for (const color of pixels) {
        if (color !== currentColor) {
            segments++;
            currentColor = color;
        }
    }
    
    // The minimal operations is (total segments - 1)
    // Because each operation can merge one segment into another
    return segments - 1;
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