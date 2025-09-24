
/**
 * Given a sequence of N piles of stones numbered from 1 to N, with each pile containing a positive number of stones,
 * determine the winner of a game played by Charlie and Dan. Charlie goes first, and they take turns moving a 
 * positive number of stones from the leftmost non-empty pile to the adjacent right pile. The player who can 
 * only move stones in the last pile loses. Assuming both players use the best possible strategy, find out 
 * who will win the game.
 *
 * @example
 * gameWinner([3, 1, 2, 2]) // returns "Dan"
 */

function gameWinner(piles) {
    const totalStones = piles.reduce((sum, pile) => sum + pile, 0);
    const numPiles = piles.length;
    // The game is won by the player who can force the opponent into a position where the remaining moves are 1.
    // The total moves is totalStones - (numPiles - 1), because each move reduces the total stones by 1 (since you move at least one stone).
    // But actually, the total moves is totalStones - (numPiles - 1), because initially, there are (numPiles - 1) gaps between piles.
    // Each move fills one gap (by moving stones from left to right), so the total moves is totalStones - (numPiles - 1).
    // The player who makes the last move (when only one pile remains) loses. So if the total moves is odd, first player wins; else second.
    const totalMoves = totalStones - (numPiles - 1);
    if (totalMoves % 2 === 1) {
        return "Charlie";
    } else {
        return "Dan";
    }
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