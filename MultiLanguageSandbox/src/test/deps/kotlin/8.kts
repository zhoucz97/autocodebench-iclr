
/**
 * Calculates the bonus based on the given profit amount. The bonus is calculated on a tiered system:
 * - For profit up to 100,000, the bonus is 10%.
 * - For profit above 100,000 and up to 200,000, it's 10% for the first 100,000 and 7.5% for the rest.
 * - For profit between 200,000 and 400,000, it's 5% for the amount above 200,000.
 * - For profit between 400,000 and 600,000, it's 3% for the amount above 400,000.
 * - For profit between 600,000 and 1,000,000, it's 1.5% for the amount above 600,000.
 * - For profit above 1,000,000, it's 1% for the amount over 1,000,000. 
 * The function returns the total bonus amount.
 * Example:
 * >>> calculateBonus(50_000.00)
 *     5000.00
 * >>> calculateBonus(150_000.00)
 *     13750.00
 */
fun calculateBonus(profit: Double): Double 
{
    var bonus = 0.0
    
    if (profit <= 100_000) {
        bonus = profit * 0.10
    } else if (profit <= 200_000) {
        bonus = 100_000 * 0.10 + (profit - 100_000) * 0.075
    } else if (profit <= 400_000) {
        bonus = 100_000 * 0.10 + 100_000 * 0.075 + (profit - 200_000) * 0.05
    } else if (profit <= 600_000) {
        bonus = 100_000 * 0.10 + 100_000 * 0.075 + 200_000 * 0.05 + (profit - 400_000) * 0.03
    } else if (profit <= 1_000_000) {
        bonus = 100_000 * 0.10 + 100_000 * 0.075 + 200_000 * 0.05 + 200_000 * 0.03 + (profit - 600_000) * 0.015
    } else {
        bonus = 100_000 * 0.10 + 100_000 * 0.075 + 200_000 * 0.05 + 200_000 * 0.03 + 400_000 * 0.015 + (profit - 1_000_000) * 0.01
    }
    
    return bonus
}
fun main(){
    check(calculateBonus(50_000.00) == 5000.00)
    check(calculateBonus(210_000.00) == 18000.00)
    check(calculateBonus(150_000.00) == 13750.00)
    check(calculateBonus(250_000.00) == 20000.00)
    check(calculateBonus(450_000.00) == 29000.00)
    check(calculateBonus(650_000.00) == 34250.00)
    check(calculateBonus(1_050_000.00) == 40000.00)
    check(calculateBonus(2_000_000.00) == 49500.00)
}

main()