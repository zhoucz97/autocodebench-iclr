
/**
 * Given n positive integers representing the count of each number from 1 to n, 
 * find the maximum sum of the mode (most frequent element) for all prefixes of 
 * a sequence constructed from these numbers. The mode is the largest number 
 * among the most frequent elements in a sequence.
 * For example:
 *   maxModeSum(3, [1, 2, 3])
 *   A sequence that reaches its maximum value is [3, 2, 3, 1, 2, 2], and the sum is 17.
 */

function maxModeSum(n, counts) {
    let sum = 0;
    let currentMaxMode = 0;
    let totalNumbers = counts.reduce((a, b) => a + b, 0);
    let sequence = [];
    
    // We'll use a priority queue (max-heap) to always pick the highest available number with remaining counts
    // In JavaScript, we can simulate this with an array and sorting
    
    // Create an array of objects {number, count} for numbers 1..n
    let numbers = [];
    for (let i = 0; i < n; i++) {
        numbers.push({ num: i + 1, count: counts[i] });
    }
    
    for (let i = 0; i < totalNumbers; i++) {
        // Sort the numbers in descending order of their value, then by remaining count (though value is primary)
        numbers.sort((a, b) => b.num - a.num);
        
        // Find the first number with remaining count > 0
        let selected = null;
        for (let numObj of numbers) {
            if (numObj.count > 0) {
                selected = numObj;
                break;
            }
        }
        
        if (selected) {
            sequence.push(selected.num);
            selected.count--;
            
            // Now, compute the mode of the current sequence
            let freq = {};
            let maxFreq = 0;
            let mode = 0;
            
            for (let num of sequence) {
                freq[num] = (freq[num] || 0) + 1;
                if (freq[num] > maxFreq || (freq[num] === maxFreq && num > mode)) {
                    maxFreq = freq[num];
                    mode = num;
                }
            }
            
            sum += mode;
        }
    }
    
    return sum;
}
(function testMaxModeSum() {
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