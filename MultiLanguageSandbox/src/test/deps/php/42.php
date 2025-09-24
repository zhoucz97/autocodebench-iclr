<?php


/**
 * Calculates the total number of cigarettes Peter can smoke.
 * Peter starts with a certain number of cigarettes and can exchange a specific number of cigarette butts for a new cigarette.
 * This process repeats until he can no longer exchange butts for new cigarettes.
 *
 * Parameters:
 * - $initialCigarettes (int): The initial number of cigarettes Peter has.
 * - $buttsRequiredForNew (int): The number of cigarette butts required to exchange for a new cigarette.
 *
 * Returns:
 * int: The total number of cigarettes Peter can smoke.
 *
 * Example:
 * - With 4 initial cigarettes and 3 butts required for a new one, Peter can smoke 5 cigarettes in total.
 * - With 10 initial cigarettes and 3 butts required for a new one, Peter can smoke 14 cigarettes in total.
 */
function calculateTotalCigarettes($initialCigarettes, $buttsRequiredForNew): int {

    $total = 0;
    $butts = 0;
    
    while ($initialCigarettes > 0) {
        // Smoke all available cigarettes
        $total += $initialCigarettes;
        $butts += $initialCigarettes;
        
        // Exchange butts for new cigarettes
        $newCigarettes = intdiv($butts, $buttsRequiredForNew);
        $butts = $butts % $buttsRequiredForNew;
        
        // Update initial cigarettes for next iteration
        $initialCigarettes = $newCigarettes;
    }
    
    return $total;
}
function main(){
    assert(calculateTotalCigarettes(4, 3) === 5, 'Test Case 1 Failed');
    assert(calculateTotalCigarettes(10, 3) === 14, 'Test Case 2 Failed');
    assert(calculateTotalCigarettes(1, 2) === 1, 'Test Case 3 Failed');
    assert(calculateTotalCigarettes(20, 5) === 24, 'Test Case 4 Failed');
    assert(calculateTotalCigarettes(0, 3) === 0, 'Test Case 5 Failed');
}


main();
?>