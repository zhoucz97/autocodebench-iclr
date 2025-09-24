
/**
 * You are playing a game called Nim. In this game, you start with a pile
 * of n stones, and you and your opponent take turns to remove 1 to 3
 * stones from the pile. The one who removes the last stone wins the game.
 * Given the number of stones n, determine if you can win the game if both
 * you and your opponent play optimally.
 * Note: You always take the first turn.
 *
 * Here are some cases:
 *   canWinNim(1) // returns true
 */

function canWinNim(n) {
    return n % 4 !== 0;
}
function testCanWinNim() {
    console.assert(canWinNim(1) === true, "Test case 1 failed");
    console.assert(canWinNim(2) === true, "Test case 2 failed");
    console.assert(canWinNim(3) === true, "Test case 3 failed");
    console.assert(canWinNim(4) === false, "Test case 4 failed");
    console.assert(canWinNim(5) === true, "Test case 5 failed");
    console.assert(canWinNim(6) === true, "Test case 6 failed");
    console.assert(canWinNim(7) === true, "Test case 7 failed");
    console.assert(canWinNim(8) === false, "Test case 8 failed");
    console.assert(canWinNim(9) === true, "Test case 9 failed");
    console.assert(canWinNim(10) === true, "Test case 10 failed");
    console.assert(canWinNim(11) === true, "Test case 11 failed");
    console.assert(canWinNim(12) === false, "Test case 12 failed");
    console.assert(canWinNim(13) === true, "Test case 13 failed");
    console.assert(canWinNim(14) === true, "Test case 14 failed");
    console.assert(canWinNim(15) === true, "Test case 15 failed");
    console.assert(canWinNim(16) === false, "Test case 16 failed");
    console.assert(canWinNim(17) === true, "Test case 17 failed");
    console.assert(canWinNim(18) === true, "Test case 18 failed");
    console.assert(canWinNim(19) === true, "Test case 19 failed");
    console.assert(canWinNim(20) === false, "Test case 20 failed");
}
testCanWinNim();