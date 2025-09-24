
/**
 * Calculate the sum of even numbers in a given list.
 * Parameters:
 * - numbers (Array): An array of integers.
 * - size (Number): The size of the array.
 * Returns:
 *   Number: The sum of even numbers in the input array.
 * Example usage:
 *   calculateEvenSum([1, 4, 3, 2, 5], 5)  // returns 6
 */

function calculateEvenSum(numbers, size) {
    let sum = 0;
    for (let i = 0; i < size; i++) {
        if (numbers[i] % 2 === 0) {
            sum += numbers[i];
        }
    }
    return sum;
}

// Example usage:
  // Output: 6
const testCalculateEvenSum = () => {
    console.assert(calculateEvenSum([1, 4, 3, 2, 5], 5) === 6);
    console.assert(calculateEvenSum([2, 2, 0, 0], 4) === 4);
    console.assert(calculateEvenSum([7, 11, 19], 3) === 0); // Should return 0 because there are no even numbers
    console.assert(calculateEvenSum([12, 14, 16, 18, 20], 5) === (12 + 14 + 16 + 18 + 20));

    // console.log("All tests passed successfully.");
}

testCalculateEvenSum();