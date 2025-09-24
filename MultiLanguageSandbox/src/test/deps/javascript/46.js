
/**
 * Given an integer n, write a function to compute the sum of all numbers from 1 to n
 * that are multiples of either 3 or 5. If a number is a multiple of both 3 and 5,
 * it should only be counted once.
 * For example:
 *     sumOfMultiples(10); // returns 33 (3 + 5 + 6 + 9 + 10)
 */

function sumOfMultiples(n) {
    let sum = 0;
    for (let i = 1; i <= n; i++) {
        if (i % 3 === 0 || i % 5 === 0) {
            sum += i;
        }
    }
    return sum;
}

// Example usage:
 // Output: 33
const testSumOfMultiples = () => {
    console.assert(sumOfMultiples(10) === 33, 'Test case n=10 failed');
    console.assert(sumOfMultiples(15) === 60, 'Test case n=15 failed');
    console.assert(sumOfMultiples(20) === 98, 'Test case n=20 failed');
    console.assert(sumOfMultiples(5) === 8, 'Test case n=5 failed');
    console.assert(sumOfMultiples(3) === 3, 'Test case n=3 failed');
    console.assert(sumOfMultiples(6) === 14, 'Test case n=6 failed');
    console.assert(sumOfMultiples(9) === 23, 'Test case n=9 failed');
    console.assert(sumOfMultiples(12) === 45, 'Test case n=12 failed');
    console.assert(sumOfMultiples(17) === 60, 'Test case n=17 failed');
    console.assert(sumOfMultiples(21) === 119, 'Test case n=21 failed');
    console.assert(sumOfMultiples(25) === 168, 'Test case n=25 failed');
};

testSumOfMultiples();