import kotlin.math.abs
/**
    * Sum the given list of fractions and simplify the resulting fraction.
    * The simplification process involves finding the greatest common divisor (GCD)
    * of the numerator and the denominator and dividing both by this GCD. 
    * If the denominator after simplification is 1, only the numerator is returned.
    *
    * Example:
    * >>> sumAndSimplifyFractions(listOf("1/2", "1/3"))
        "5/6"
    * >>> sumAndSimplifyFractions(listOf("1/6", "1/4", "1/3"))
        "3/4"
**/



fun sumAndSimplifyFractions(fractions: List<String>): String
{
    if (fractions.isEmpty()) return "0"

    // Parse all fractions into numerators and denominators
    val parsedFractions = fractions.map { fraction ->
        val parts = fraction.split('/')
        Pair(parts[0].toInt(), parts[1].toInt())
    }

    // Sum all fractions
    var sumNumerator = 0
    var sumDenominator = 1

    for ((numerator, denominator) in parsedFractions) {
        sumNumerator = sumNumerator * denominator + numerator * sumDenominator
        sumDenominator *= denominator
    }

    // Simplify the fraction
    val gcdValue = gcd(abs(sumNumerator), abs(sumDenominator))
    val simplifiedNumerator = sumNumerator / gcdValue
    val simplifiedDenominator = sumDenominator / gcdValue

    // Return the result
    return if (simplifiedDenominator == 1) {
        simplifiedNumerator.toString()
    } else {
        "$simplifiedNumerator/$simplifiedDenominator"
    }
}

// Helper function to calculate GCD using Euclidean algorithm
fun gcd(a: Int, b: Int): Int {
    var num1 = a
    var num2 = b
    while (num2 != 0) {
        val temp = num2
        num2 = num1 % num2
        num1 = temp
    }
    return num1
}
fun main() {
    check(sumAndSimplifyFractions(listOf("1/2", "1/3")) == "5/6")
    check(sumAndSimplifyFractions(listOf("1/6", "1/4", "1/3")) == "3/4")
    check(sumAndSimplifyFractions(listOf("1/8", "3/8")) == "1/2")
    check(sumAndSimplifyFractions(listOf("2/3", "1/3")) == "1")
}


main()