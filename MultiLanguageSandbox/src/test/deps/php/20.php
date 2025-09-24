<?php
/**
 * Determines the ranking of a player at the time of their entry in a competition.
 * 
 * In this competition, scores are announced one after another, updating the leaderboard each time.
 * This function calculates the rank of a specific player at the time they were added to the leaderboard.
 * 
 * Input: 
 * - $scores: An array of integers representing the scores of each player.
 * - $playerIndex: An integer representing the index of the player (1-based index).
 * 
 * Output: 
 * - Returns an integer, the rank of the player at the time of their entry.
 * 
 * Example: 
 * For scores [95, 92, 93, 100, 102, 99] and playerIndex 3, the function should return 2.
 */


function getPlayerRankingAtEntry($scores, $playerIndex) {

    // Convert playerIndex to 0-based if necessary (since PHP arrays are 0-based)
    $playerPos = $playerIndex - 1;
    
    // Extract scores up to and including the player's position
    $relevantScores = array_slice($scores, 0, $playerPos + 1);
    
    // Sort the relevant scores in descending order
    rsort($relevantScores);
    
    // Find the rank of the player's score in the sorted list
    $playerScore = $scores[$playerPos];
    $rank = 1;
    foreach ($relevantScores as $score) {
        if ($score > $playerScore) {
            $rank++;
        } else {
            break; // since the list is sorted, further elements won't be higher
        }
    }
    
    return $rank;
}
function main(){
    assert(getPlayerRankingAtEntry([95, 92, 93, 100, 102, 99], 3) === 2);
    assert(getPlayerRankingAtEntry([90, 85, 95, 88, 92], 5) === 2);
    assert(getPlayerRankingAtEntry([100, 105, 95, 110, 90], 1) === 1);
    assert(getPlayerRankingAtEntry([70, 80, 60, 90, 85], 4) === 1);
    assert(getPlayerRankingAtEntry([95, 95, 95, 95, 95], 3) === 3);
    
}


main();
?>