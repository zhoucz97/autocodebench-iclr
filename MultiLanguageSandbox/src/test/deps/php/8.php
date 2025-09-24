<?php

/**
 * Compares two playing cards to determine which one is higher based on a given trump suit.
 * In this game, each card is represented by its suit and rank (e.g., '8D', 'QS').
 * The trump suit is one of the four suits: Spades (S), Hearts (H), Diamonds (D), or Clubs (C).
 * The rules for comparison are as follows:
 * - If neither card is of the trump suit, the card with the higher rank wins.
 * - If both cards are of the trump suit, the card with the higher rank wins.
 * - If one card is of the trump suit and the other is not, the trump suit card wins regardless of rank.
 * - If both cards have the same rank, the result is a draw.
 * Input: The trump suit and two card strings.
 * Output: Returns 1 if the first card wins, -1 if the second card wins, and 0 for a draw.
 */

function compareCards($trumpSuit, $card1, $card2) {

    // Extract suit and rank from each card
    $suit1 = substr($card1, -1);
    $rank1 = substr($card1, 0, -1);
    
    $suit2 = substr($card2, -1);
    $rank2 = substr($card2, 0, -1);
    
    // Check if either card is trump
    $isTrump1 = ($suit1 == $trumpSuit);
    $isTrump2 = ($suit2 == $trumpSuit);
    
    // Rule 3: If one card is trump and the other isn't, trump wins
    if ($isTrump1 && !$isTrump2) {
        return 1;
    }
    if (!$isTrump1 && $isTrump2) {
        return -1;
    }
    
    // Rule 1/2: Both cards are trump or neither is trump - compare ranks
    $rankOrder = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A'];
    $rankIndex1 = array_search($rank1, $rankOrder);
    $rankIndex2 = array_search($rank2, $rankOrder);
    
    if ($rankIndex1 > $rankIndex2) {
        return 1;
    } elseif ($rankIndex1 < $rankIndex2) {
        return -1;
    } else {
        return 0; // Draw
    }
}
function main(){
    assert(compareCards('S', '8D', '3S') === -1); 
    assert(compareCards('S', '8D', '9S') === -1); // Non-trump vs trump
    assert(compareCards('H', '8H', '7H') === 1);  // Both trump, first higher
    assert(compareCards('D', '9C', '9S') === 0);  // Same rank, non-trump
    assert(compareCards('C', 'QH', 'JS') === 1);  // Neither trump, first higher
    assert(compareCards('D', 'KD', 'AD') === -1); // Both trump, second higher

}
main();
?>