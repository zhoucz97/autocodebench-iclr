
/**
 * Calculate the Hamming distance between two integers in binary representation.
 * Parameters:
 * - x (number): The first positive integer (x <= 1,000,000,000).
 * - y (number): The second positive integer (y <= 1,000,000,000).
 * Returns:
 * - number: The Hamming distance between x and y, i.e., the number of bits
 *           that need to be flipped to transform x into y in binary.
 * Example usage:
 *   hamming_distance(1, 2); // returns 2
 *   hamming_distance(4, 7); // returns 2
 */

function hamming_distance(x: number, y: number): number {
    let xor = x ^ y; // XOR operation to find differing bits
    let distance = 0;
    
    while (xor !== 0) {
        distance += xor & 1; // Check the least significant bit
        xor >>>= 1; // Right shift by 1 (unsigned)
    }
    
    return distance;
}
(() => {
    console.assert(hamming_distance(1, 2) === 2, '01 and 10 have 2 different bits');
    console.assert(hamming_distance(4, 7) === 2, '100 and 111 have 2 different bits');
    console.assert(hamming_distance(25, 30) === 3, '11001 and 11110 have 3 different bits');
    console.assert(hamming_distance(0, 0) === 0, 'Same numbers have 0 different bits');
    console.assert(hamming_distance(0xFFFFFFF, 0) === 28, 'Max unsigned int and 0 have 28 different bits');

    // If no assertion fails, log success message to the console
    // console.log("All tests passed!");
})();