// No import is necessary as we're not using any modules or libraries
/**
 * Determine if two closed intervals intersect.
 * 
 * @param {number} a - Start of the first closed interval, must satisfy 0 <= a <= b <= 1000.
 * @param {number} b - End of the first closed interval, must satisfy 0 <= a <= b <= 1000.
 * @param {number} c - Start of the second closed interval, must satisfy 0 <= c <= d <= 1000.
 * @param {number} d - End of the second closed interval, must satisfy 0 <= c <= d <= 1000.
 * @returns {number} 1 if the intervals intersect, 0 otherwise.
 */

function areIntervalsIntersecting(a, b, c, d) {
    // Check if the intervals intersect
    if (b >= c && d >= a) {
        return 1; // Intervals intersect
    } else {
        return 0; // Intervals do not intersect
    }
}
const testAreIntervalsIntersecting = () => {
    console.assert(areIntervalsIntersecting(1, 1, 1, 2) === 1);
    console.assert(areIntervalsIntersecting(3, 5, 2, 6) === 1);
    console.assert(areIntervalsIntersecting(3, 5, 4, 7) === 1);
    console.assert(areIntervalsIntersecting(3, 5, 6, 7) === 0);
    // Additional test cases
    console.assert(areIntervalsIntersecting(0, 0, 0, 0) === 1);
    console.assert(areIntervalsIntersecting(1, 3, 2, 4) === 1);
    console.assert(areIntervalsIntersecting(1, 3, 4, 6) === 0);
    console.assert(areIntervalsIntersecting(10, 20, 20, 30) === 1);
    console.assert(areIntervalsIntersecting(10, 20, 21, 30) === 0);

    // console.log("All tests passed.");
}

testAreIntervalsIntersecting();