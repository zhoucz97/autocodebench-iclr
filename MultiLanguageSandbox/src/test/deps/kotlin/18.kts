
/**
    * Determines if a given integer is a perfect number. A perfect number is one whose
    * sum of all positive divisors (excluding itself) equals the number itself.
    * 
    * Examples:
    * - isPerfectNumber(6) returns true because 1 + 2 + 3 = 6
    * - isPerfectNumber(28) returns true because 1 + 2 + 4 + 7 + 14 = 28
    * - isPerfectNumber(10) returns false because 1 + 2 + 5 ≠ 10
**/


fun isPerfectNumber(number: Int): Boolean 
{
    if (number <= 1) return false // Perfect numbers are greater than 1
    
    var sum = 1 // 1 is a proper divisor for all numbers > 1
    val sqrt = Math.sqrt(number.toDouble()).toInt()
    
    for (i in 2..sqrt) {
        if (number % i == 0) {
            sum += i
            val otherDivisor = number / i
            if (otherDivisor != i) {
                sum += otherDivisor
            }
        }
    }
    
    return sum == number
}
fun main(){
    check(isPerfectNumber(1) == false)  // 6 is a perfect number
    check(isPerfectNumber(6) == true)  // 6 is a perfect number
    check(isPerfectNumber(28) == true)  // 28 is a perfect number
    check(isPerfectNumber(10) == false)  // 10 is not a perfect number
    check(isPerfectNumber(496) == true)  // 496 is a perfect number
    check(isPerfectNumber(8128) == true)  // 8128 is a perfect number
    check(isPerfectNumber(15) == false)  // 15 is not a perfect number
    check(isPerfectNumber(20) == false)  // 20 is not a perfect number


}
main()