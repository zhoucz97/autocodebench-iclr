
/**
 * Given n positive integers representing the count of each number from 1 to n, 
 * find the maximum sum of the mode (most frequent element) for all prefixes of 
 * a sequence constructed from these numbers. The mode is the largest number 
 * among the most frequent elements in a sequence.
 * For example:
 *   maxModeSum(3, [1, 2, 3])
 *   A sequence that reaches its maximum value is [3, 2, 3, 1, 2, 2], and the sum is 17.
 */

function maxModeSum(n: number, counts: number[]): number {
    // Create an array of tuples [number, count], then sort in descending order of the number
    const numCounts: [number, number][] = [];
    for (let i = 0; i < n; i++) {
        numCounts.push([i + 1, counts[i]]);
    }
    // Sort the numbers in descending order to prioritize larger numbers
    numCounts.sort((a, b) => b[0] - a[0]);
    
    let sequence: number[] = [];
    for (const [num, cnt] of numCounts) {
        for (let i = 0; i < cnt; i++) {
            sequence.push(num);
        }
    }
    
    let sum = 0;
    const frequency: Map<number, number> = new Map();
    let maxFrequency = 0;
    let currentMode = 0;
    
    for (const num of sequence) {
        // Update frequency of the current number
        frequency.set(num, (frequency.get(num) || 0) + 1);
        const currentFreq = frequency.get(num)!;
        
        // Check if this number's frequency is now the highest or equal to the highest but larger number
        if (currentFreq > maxFrequency || (currentFreq === maxFrequency && num > currentMode)) {
            maxFrequency = currentFreq;
            currentMode = num;
        }
        sum += currentMode;
    }
    
    return sum;
}
(function testMaxModeSum(): void {
  console.assert(maxModeSum(3, [1, 3, 2]) === 17, 'Test case 1 failed');
  console.assert(maxModeSum(4, [4, 1, 2, 3]) === 37, 'Test case 2 failed');
  console.assert(maxModeSum(2, [1, 1]) === 4, 'Test case 3 failed');
  console.assert(maxModeSum(5, [1, 2, 3, 4, 5]) === 75, 'Test case 4 failed');
  console.assert(maxModeSum(1, [100000]) === 100000, 'Test case 5 failed');
  console.assert(maxModeSum(5, [5, 3, 2, 4, 1]) === 62, 'Test case 6 failed');
  console.assert(maxModeSum(3, [100000, 100000, 100000]) === 900000, 'Test case 7 failed');
  console.assert(maxModeSum(3, [2, 2, 5]) === 27, 'Test case 8 failed');
  console.assert(maxModeSum(4, [4, 4, 4, 4]) === 64, 'Test case 9 failed');
  console.assert(maxModeSum(6, [1, 2, 3, 4, 5, 6]) === 126, 'Test case 10 failed');
  console.assert(maxModeSum(3, [3, 1, 2]) === 16, 'Test case 11 failed');
})();