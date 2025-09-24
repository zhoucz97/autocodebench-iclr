
/**
 * Determine if two closed intervals intersect.
 * Args:
 *    a, b: Representing the first closed interval [a, b] where 0 <= a <= b <= 1000.
 *    c, d: Representing the second closed interval [c, d] where 0 <= c <= d <= 1000.
 * Returns:
 *    boolean: true if the intervals intersect, false otherwise.
 */

function areIntervalsIntersecting(a: number, b: number, c: number, d: number): boolean {
    // Check if one interval starts after the other ends
    return !(b < c || d < a);
}
(() => {
    console.assert(areIntervalsIntersecting(1, 1, 1, 2) === true);
    console.assert(areIntervalsIntersecting(3, 5, 2, 6) === true);
    console.assert(areIntervalsIntersecting(3, 5, 4, 7) === true);
    console.assert(areIntervalsIntersecting(3, 5, 6, 7) === false);
    // Additional test cases
    console.assert(areIntervalsIntersecting(0, 0, 0, 0) === true);
    console.assert(areIntervalsIntersecting(1, 3, 2, 4) === true);
    console.assert(areIntervalsIntersecting(1, 3, 4, 6) === false);
    console.assert(areIntervalsIntersecting(10, 20, 20, 30) === true);
    console.assert(areIntervalsIntersecting(10, 20, 21, 30) === false);
})();