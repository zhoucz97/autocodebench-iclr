
/**
 * Calculate the Hamming distance between two integers in their binary representation.
 *
 * Parameters:
 * - x: The first positive integer (x <= 1,000,000,000).
 * - y: The second positive integer (y <= 1,000,000,000).
 *
 * Returns:
 * The Hamming distance between x and y, i.e., the number of bits that need to be
 * flipped to transform x into y.
 * 
 * Examples:
 * hammingDistance(1, 2) // returns 2
 */

function hammingDistance(x, y) {
    let xor = x ^ y;
    let distance = 0;
    while (xor > 0) {
        distance += xor & 1;
        xor = xor >>> 1;
    }
    return distance;
}
const testHammingDistance = () => {
    console.assert(hammingDistance(1, 2) === 2, "01 and 10 have 2 different bits");
    console.assert(hammingDistance(4, 7) === 2, "100 and 111 have 2 different bits");
    console.assert(hammingDistance(25, 30) === 3, "11001 and 11110 have 3 different bits");
    console.assert(hammingDistance(0, 0) === 0, "Same numbers have 0 different bits");
    console.assert(hammingDistance(0xFFFFFFF, 0x0000000) === 28,
                   "Max unsigned int and 0 have 28 different bits");
}

// Run the test function
testHammingDistance();