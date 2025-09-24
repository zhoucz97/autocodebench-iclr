
/**
 * Counts the number of odd integers in a given list of numbers.
 * 
 * @param count - The count of numbers to evaluate.
 * @param numbers - A list of integers to be checked for oddness.
 * @returns The count of odd numbers in the input list.
 *
 * @example
 * ```
 * countOddNumbers(5, [1, 4, 3, 2, 5]) // returns 3
 * ```
 */

function countOddNumbers(count: number, numbers: number[]): number {
    let oddCount = 0;
    for (let i = 0; i < count; i++) {
        if (numbers[i] % 2 !== 0) {
            oddCount++;
        }
    }
    return oddCount;
}
const testCountOddNumbers = (): void => {
    console.assert(countOddNumbers(5, [1, 4, 3, 2, 5]) === 3, "Test 1 failed");
    console.assert(countOddNumbers(4, [2, 2, 0, 0]) === 0, "Test 2 failed");
    console.assert(countOddNumbers(6, [7, 7, 8, 1, 9, 10]) === 4, "Test 3 failed");
    // console.log("All tests passed!");
};

testCountOddNumbers();