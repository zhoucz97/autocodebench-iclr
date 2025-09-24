
/**
 * Determines the number of ways to choose a triplet (a, b, c) 
 * from an array of distinct integers representing heights such that the 
 * greatest common divisor (GCD) of the maximum and minimum values 
 * of the triplet is 1.
 * 
 * @example
 * countTriplets([1, 5, 7])
 * // Returns: 1
 */

function countTriplets(heights) {
    let count = 0;
    const n = heights.length;
    
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            for (let k = j + 1; k < n; k++) {
                const a = heights[i];
                const b = heights[j];
                const c = heights[k];
                const maxVal = Math.max(a, b, c);
                const minVal = Math.min(a, b, c);
                if (gcd(maxVal, minVal) === 1) {
                    count++;
                }
            }
        }
    }
    
    return count;
}

function gcd(a, b) {
    while (b !== 0) {
        let temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}
const main = () => {
    console.assert(countTriplets([1, 5, 7]) === 1);
    console.assert(countTriplets([1, 6, 2, 3]) === 3);
    console.assert(countTriplets([16, 4, 8, 2]) === 0);
    console.assert(countTriplets([10, 1, 6, 7, 9, 8, 4, 3, 5, 2]) === 77);
    console.assert(countTriplets([4, 5, 9, 11, 14]) === 7);
    console.assert(countTriplets([15, 10, 6, 9, 7, 3, 12, 8, 11, 1, 2]) === 104);
    console.assert(countTriplets([3, 7, 11, 13]) === 4);
    console.assert(countTriplets([5, 12, 13, 17, 19]) === 10);
    console.assert(countTriplets([2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]) === 87);
    console.assert(countTriplets([1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17]) === 122);
}

main();