
/**
 * Calculate the total number of cigarettes Peter can smoke given an initial number of cigarettes and a ratio of cigarette butts to new cigarettes.
 * Peter saves butts from smoked cigarettes and exchanges them for new ones at a given rate.
 * 
 * Example:
 * >>> totalCigarettesSmoked(4, 3)
 *     5
 * >>> totalCigarettesSmoked(10, 3)
 *     14
 */
fun totalCigarettesSmoked(initialCigarettes: Int, buttsForNew: Int): Int 
{
    var totalSmoked = 0
    var currentCigs = initialCigarettes
    var butts = 0
    
    while (currentCigs > 0) {
        // Smoke all current cigarettes
        totalSmoked += currentCigs
        butts += currentCigs
        
        // Exchange butts for new cigarettes
        val newCigs = butts / buttsForNew
        butts %= buttsForNew
        currentCigs = newCigs
    }
    
    return totalSmoked
}
fun main() {
    check(totalCigarettesSmoked(4, 3) == 5)
    check(totalCigarettesSmoked(10, 3) == 14)
    check(totalCigarettesSmoked(1, 2) == 1) // Edge case: Not enough cigarettes to exchange
    check(totalCigarettesSmoked(20, 5) == 24) // More cigarettes, different exchange rate
}

main()