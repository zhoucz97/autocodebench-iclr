
/**
 * Given n doors arranged in a circle, the player starts in front of door 1.
 * Each turn, the player can choose a number i and pay a cost C_i to move i steps
 * to the right and then open the door at that position. It is guaranteed that
 * C_i >= C_{i+1} for 1 <= i < n.
 * The task is to determine the minimum total cost required to open all doors.
 * 
 * Example:
 *     > minTotalCost(3, [1, 1, 1])
 *     3
 */

function minTotalCost(n, C) {
    return n * C[n - 1];
}
function testMinTotalCost() {
    console.assert(minTotalCost(5, [4, 3, 3, 3, 3]) === 15);
    console.assert(minTotalCost(3, [1, 1, 1]) === 3);
    console.assert(minTotalCost(4, [5, 4, 3, 2]) === 11);
    console.assert(minTotalCost(4, [100, 99, 98, 97]) === 391);
    console.assert(minTotalCost(6, [10, 9, 8, 7, 6, 5]) === 35);
    console.assert(minTotalCost(7, [2, 2, 2, 2, 2, 2, 2]) === 14);
    console.assert(minTotalCost(8, [9, 7, 7, 7, 7, 7, 7, 7]) === 56);
    console.assert(minTotalCost(9, [3, 2, 2, 2, 2, 2, 2, 2, 2]) === 18);
    console.assert(minTotalCost(10, [6, 5, 5, 5, 5, 5, 5, 5, 5, 5]) === 50);
    console.assert(minTotalCost(11, [8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]) === 11);

}

testMinTotalCost();