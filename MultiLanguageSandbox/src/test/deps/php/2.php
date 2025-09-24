<?php


/**
    * Determines the outcome of a "Rock, Paper, Scissors" game.
    * In this game, each player selects either rock (0), paper (1), or scissors (2).
    * The rules are as follows:
    * - Paper beats rock
    * - Rock beats scissors
    * - Scissors beats paper
    * If both players choose the same item, it's a draw.
    * Input: Two integers representing the choices of two players.
    * Output: Returns 1 if the first player wins, -1 if the second player wins, and 0 for a draw.
*/

function determineGameOutcome($player1, $player2) {

    // Check for a draw
    if ($player1 == $player2) {
        return 0;
    }

    // Determine the winner based on the rules
    if (($player1 == 0 && $player2 == 2) || // Rock beats scissors
        ($player1 == 1 && $player2 == 0) || // Paper beats rock
        ($player1 == 2 && $player2 == 1)) { // Scissors beat paper
        return 1; // Player 1 wins
    } else {
        return -1; // Player 2 wins
    }
}
function main(){
    
    assert(determineGameOutcome(0, 1) === -1);
    assert(determineGameOutcome(1, 2) === -1);
    assert(determineGameOutcome(2, 0) === -1);
    assert(determineGameOutcome(0, 2) === 1);
    assert(determineGameOutcome(1, 0) === 1);
    assert(determineGameOutcome(2, 1) === 1);
    assert(determineGameOutcome(0, 0) === 0);
    assert(determineGameOutcome(1, 1) === 0);
    assert(determineGameOutcome(2, 2) === 0);
}
main();
// echo "All test passed!\n";
?>