
/**
 * Given a sequence of N piles of stones numbered from 1 to N, each containing a positive number of stones,
 * this function identifies the winner of a game played by Charlie and Dan. The game starts with Charlie,
 * and they alternate turns moving stones from the leftmost non-empty pile to the adjacent right pile.
 * Movement is done in positive numbers. The player forced to only move stones in the last pile loses.
 * Assuming optimal play, the function determines the winner.
 *
 * @example
 * gameWinner([3, 1, 2, 2]) // returns "Dan"
 */

function gameWinner(piles: number[]): string {
    const totalStones = piles.reduce((sum, pile) => sum + pile, 0);
    const numPiles = piles.length;
    const moves = totalStones - numPiles;
    
    return moves % 2 === 0 ? "Charlie" : "Dan";
}
(() => {
    console.assert(gameWinner([1, 2, 2]) === "Dan", "Test 1 failed");
    console.assert(gameWinner([5, 5, 5, 5, 5]) === "Charlie", "Test 2 failed");
    console.assert(gameWinner([2, 1, 2]) === "Charlie", "Test 3 failed");
    console.assert(gameWinner([3, 3, 3, 3]) === "Charlie", "Test 4 failed");
    console.assert(gameWinner([1, 1]) === "Charlie", "Test 5 failed");
    console.assert(gameWinner([2, 1]) === "Charlie", "Test 6 failed");
    console.assert(gameWinner([1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]) === "Dan", "Test 7 failed");
    console.assert(gameWinner([2, 2, 2, 2, 2, 2, 2, 2, 2, 1]) === "Charlie", "Test 8 failed");
    console.assert(gameWinner([10, 10, 10, 10, 10, 10, 10, 10, 10, 10]) === "Charlie", "Test 9 failed");
    console.assert(gameWinner([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) === "Dan", "Test 10 failed");
})();