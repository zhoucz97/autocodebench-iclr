<?php



/**
 * Calculates the total earnings of Deliv-e-droid based on the number of successfully delivered and failed deliveries.
 * Deliv-e-droid earns 50 units of money for each successful delivery and loses 10 units for each failed delivery.
 * Additionally, if the number of successful deliveries is greater than the number of failed ones, Deliv-e-droid receives a bonus of 500 units.
 * 
 * Arguments:
 * - $delivered: The number of successfully delivered packages (0 <= $delivered <= 100).
 * - $failed: The number of failed deliveries (0 <= $failed <= 100).
 * 
 * Returns:
 * - The total earnings of Deliv-e-droid, which can be negative if the losses outweigh the gains.
 * 
 * Examples:
 * - calculateDroidEarnings(5, 2) returns 730.
 * - calculateDroidEarnings(0, 10) returns -100.
 */

 function calculateDroidEarnings(int $delivered, int $failed): int {

    // Calculate earnings from successful deliveries
    $earnings = $delivered * 50;
    
    // Subtract losses from failed deliveries
    $earnings -= $failed * 10;
    
    // Add bonus if successful deliveries are greater than failed ones
    if ($delivered > $failed) {
        $earnings += 500;
    }
    
    return $earnings;
}
function main(){
    assert(calculateDroidEarnings(5, 2) === 730, 'Test case 1 failed');
    assert(calculateDroidEarnings(0, 10) === -100, 'Test case 2 failed');
    assert(calculateDroidEarnings(10, 0) === 1000, 'Test case 3 failed');
    assert(calculateDroidEarnings(3, 3) === 120, 'Test case 4 failed');
    assert(calculateDroidEarnings(0, 0) === 0, 'Test case 5 failed');
    
}


main();
?>