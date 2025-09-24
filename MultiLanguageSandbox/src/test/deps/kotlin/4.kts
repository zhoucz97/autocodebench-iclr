
/**
 * Given an integer 'n', find and return the count of numbers between 11 and 'n' (inclusive)
 * that are both prime and palindrome.
 *
 * Example:
 * >>> countPrimePalindromeNumbers(50)
 * 2 // Because there are two numbers, 11 and 22, that are prime and palindrome between 11 and 50.
 *
 * Note:
 * - A prime number is a positive integer greater than 1 that has no positive integer divisors other than 1 and itself.
 * - A palindrome number is a number that reads the same forwards and backwards.
 */


fun countPrimePalindromeNumbers(n: Int): Int 
{
    var count = 0
    for (num in 11..n) {
        if (isPalindrome(num) && isPrime(num)) {
            count++
        }
    }
    return count
}

fun isPalindrome(number: Int): Boolean {
    val str = number.toString()
    return str == str.reversed()
}

fun isPrime(number: Int): Boolean {
    if (number <= 1) return false
    if (number == 2) return true
    if (number % 2 == 0) return false
    for (i in 3..Math.sqrt(number.toDouble()).toInt() step 2) {
        if (number % i == 0) {
            return false
        }
    }
    return true
}
fun main() {
    check(countPrimePalindromeNumbers(50) == 1) // There are two prime palindrome numbers between 11 and 50 (11).
    check(countPrimePalindromeNumbers(150) == 3) // There are three prime palindrome numbers between 11 and 100 (11, 101, 131).
    check(countPrimePalindromeNumbers(200) == 6) // There are four prime palindrome numbers between 11 and 200 (11, 101, 131, 151, 181, 191).
    check(countPrimePalindromeNumbers(11) == 1)  // The only prime palindrome number between 11 and 11 is 11 itself.
}

main()