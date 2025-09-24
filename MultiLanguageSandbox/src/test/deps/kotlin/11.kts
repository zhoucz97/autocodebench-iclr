
/**
    * Calculates the total savings accumulated over a period of months with monthly deposits and a fixed interest rate.
    * The interest is compounded monthly based on the total amount in the account at the end of each month.
    Example:
    >>> calculateSavingsInterest(1000.0, 0.01, 6)  // 1000 monthly deposit, 1% monthly interest, over 6 months
        6213.54
    >>> calculateSavingsInterest(500.0, 0.02, 12)  // 500 monthly deposit, 2% monthly interest, over 12 months
        6840.17
**/

fun calculateSavingsInterest(monthlyDeposit: Double, interestRate: Double, months: Int): Double 
{
    var totalAmount = 0.0
    for (month in 1..months) {
        totalAmount += monthlyDeposit
        totalAmount *= (1 + interestRate)
    }
    return totalAmount
}
fun main(){
    check(calculateSavingsInterest(1000.0, 0.01, 6) == 6213.54)
    check(calculateSavingsInterest(500.0, 0.02, 12) == 6840.17)
    check(calculateSavingsInterest(2000.0, 0.005, 3) == 6060.2)
    check(calculateSavingsInterest(750.0, 0.01, 10) == 7925.13)
}
main()