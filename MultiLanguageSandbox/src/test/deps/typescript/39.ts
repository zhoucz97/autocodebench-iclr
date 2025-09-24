
/**
 * Given an array A of integers, the task is to calculate the sum of the XOR of all subarrays.
 * A subarray is defined by a pair of indices (L, R) such that 1 <= L <= R <= the length of the array.
 * The XOR sum of a subarray is the result of XORing all elements from L to R.
 * The final result is the sum of the XOR sums for all possible subarrays.
 *
 * Example:
 * sumOfXorSubarrays([1, 2, 3, 4, 5])  // returns 39
 */

function sumOfXorSubarrays(A: number[]): number {
    let totalSum = 0;
    const n = A.length;
    
    for (let L = 0; L < n; L++) {
        let currentXor = 0;
        for (let R = L; R < n; R++) {
            currentXor ^= A[R];
            totalSum += currentXor;
        }
    }
    
    return totalSum;
}
const testSumOfXorSubarrays = (): void => {
    console.assert(sumOfXorSubarrays([1, 2, 3, 4, 5]) === 39);
    console.assert(sumOfXorSubarrays([1, 1, 1]) === 4);
    console.assert(sumOfXorSubarrays([2, 3, 1]) === 9);
    console.assert(sumOfXorSubarrays([4, 5, 7, 9]) === 74);
    console.assert(sumOfXorSubarrays([0, 0, 0, 0]) === 0);
    console.assert(sumOfXorSubarrays([8, 8, 8, 8, 8]) === 72);
    console.assert(sumOfXorSubarrays([3, 6, 9, 12, 15]) === 125);
    console.assert(sumOfXorSubarrays([10, 20, 30, 40, 50]) === 390);
    console.assert(sumOfXorSubarrays([16, 16, 16, 16, 16, 16]) === 192);
    console.assert(sumOfXorSubarrays([1, 3, 5, 7, 9, 11, 13]) === 192);
    console.assert(sumOfXorSubarrays([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) === 218);
};

testSumOfXorSubarrays();