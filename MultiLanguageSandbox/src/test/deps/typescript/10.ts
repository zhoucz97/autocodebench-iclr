
/**
 * Calculate the sum of even numbers in a given list.
 * 
 * @param numbers An array of integers.
 * @param size The size of the array.
 * @returns The sum of even numbers in the input array.
 * 
 * Examples:
 * 
 * calculateEvenSum([1, 4, 3, 2, 5], 5); // Returns 6
 * calculateEvenSum([2, 2, 0, 0], 4); // Returns 4
 * calculateEvenSum([7, 11, 19], 3); // Returns 0 (no even numbers)
 */

function calculateEvenSum(numbers: number[], size: number): number {
    let sum = 0;
    for (let i = 0; i < size; i++) {
        if (numbers[i] % 2 === 0) {
            sum += numbers[i];
        }
    }
    return sum;
}
(() => {
    console.assert(calculateEvenSum([1, 4, 3, 2, 5], 5) === 6, "Test 1 failed");
    console.assert(calculateEvenSum([2, 2, 0, 0], 4) === 4, "Test 2 failed");
    console.assert(calculateEvenSum([7, 11, 19], 3) === 0, "Test 3 failed - no even numbers");
    console.assert(calculateEvenSum([12, 14, 16, 18, 20], 5) === (12 + 14 + 16 + 18 + 20), "Test 4 failed - all even numbers");
    // console.log("All tests passed successfully.");
})();