
/**
 * Find the length of the longest consecutive sequence of 1s in the binary representation of a non-negative integer.
 *
 * Parameters:
 * - n (number): A non-negative integer (0 ≤ n ≤ 2^64 - 1).
 *
 * Returns:
 * - number: The length of the longest consecutive sequence of 1s in the binary representation of the given integer.
 *
 * Examples:
 * extraNumber(7) returns 3
 * extraNumber(13) returns 2
 * extraNumber(12345) returns 3
 * extraNumber(0b11011101111) returns 4
 * extraNumber(0xFFFFFFFF) returns 32
 * extraNumber(0) returns 0
 */

function findLongestConsecutiveOnesLength(n: number): number {
    if (n === 0) return 0;
    const binaryString = n.toString(2);
    let maxLength = 0;
    let currentLength = 0;
    
    for (const char of binaryString) {
        if (char === '1') {
            currentLength++;
            if (currentLength > maxLength) {
                maxLength = currentLength;
            }
        } else {
            currentLength = 0;
        }
    }
    
    return maxLength;
}
const testFindLongestConsecutiveOnesLength = (): void => {
    console.assert(findLongestConsecutiveOnesLength(7) === 3, "Failed on input 7");
    console.assert(findLongestConsecutiveOnesLength(13) === 2, "Failed on input 13");
    console.assert(findLongestConsecutiveOnesLength(12345) === 3, "Failed on input 12345");
    console.assert(findLongestConsecutiveOnesLength(0b11011101111) === 4, "Failed on input 0b11011101111");
    console.assert(findLongestConsecutiveOnesLength(0xFFFFFFFF) === 32, "Failed on input 0xFFFFFFFF");
    console.assert(findLongestConsecutiveOnesLength(0) === 0, "Failed on input 0");

    // console.log("All tests passed!");
};

testFindLongestConsecutiveOnesLength();