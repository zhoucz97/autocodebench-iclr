
/**
 * Find the length of the longest consecutive sequence of 1s in the binary 
 * representation of a non-negative integer.
 *
 * @param {number} n - A non-negative integer (0 ≤ n ≤ 2^64 - 1).
 * @returns {number} The length of the longest consecutive sequence of 1s in 
 *                   the binary representation of the given integer.
 * 
 * Examples:
 * findLongestConsecutiveOnesLength(7) === 3
 * findLongestConsecutiveOnesLength(13) === 2
 */

function findLongestConsecutiveOnesLength(n) {
    const binaryString = n.toString(2);
    let maxCount = 0;
    let currentCount = 0;
    
    for (const char of binaryString) {
        if (char === '1') {
            currentCount++;
            if (currentCount > maxCount) {
                maxCount = currentCount;
            }
        } else {
            currentCount = 0;
        }
    }
    
    return maxCount;
}
const testFindLongestConsecutiveOnesLength = () => {
    console.assert(findLongestConsecutiveOnesLength(7) === 3, "Test with n = 7");
    console.assert(findLongestConsecutiveOnesLength(13) === 2, "Test with n = 13");
    console.assert(findLongestConsecutiveOnesLength(12345) === 3, "Test with n = 12345");
    console.assert(findLongestConsecutiveOnesLength(0b11011101111) === 4, "Test with n = 0b11011101111");
    console.assert(findLongestConsecutiveOnesLength(0xFFFFFFFF) === 32, "Test with n = 0xFFFFFFFF");
    console.assert(findLongestConsecutiveOnesLength(0) === 0, "Test with n = 0");

    // console.log("All tests passed!");
}

testFindLongestConsecutiveOnesLength();