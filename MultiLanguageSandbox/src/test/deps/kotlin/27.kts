
/**
    * Finds all prime pairs where each prime is less than or equal to a given number and the pair differs by 2.
    * A prime pair is defined as two prime numbers where the difference between them is exactly 2.
    * Example:
    * >>> findPrimePairs(10)
        [(3, 5), (5, 7)]
    * >>> findPrimePairs(100)
        [(3, 5), (5, 7), (11, 13), (17, 19), (29, 31), (41, 43), (59, 61), (71, 73)]
**/

fun findPrimePairs(maxNumber: Int): List<Pair<Int, Int>> 
{
    val primes = mutableListOf<Int>()
    for (num in 2..maxNumber) {
        if (isPrime(num)) {
            primes.add(num)
        }
    }
    
    val primePairs = mutableListOf<Pair<Int, Int>>()
    for (i in 0 until primes.size - 1) {
        val current = primes[i]
        val next = primes[i + 1]
        if (next - current == 2) {
            primePairs.add(current to next)
        }
    }
    
    return primePairs
}

fun isPrime(n: Int): Boolean {
    if (n <= 1) return false
    if (n == 2) return true
    if (n % 2 == 0) return false
    
    for (i in 3..Math.sqrt(n.toDouble()).toInt() step 2) {
        if (n % i == 0) return false
    }
    return true
}
fun main(){
    check(findPrimePairs(10) == listOf(Pair(3, 5), Pair(5, 7)))
    check(findPrimePairs(100) == listOf(Pair(3, 5), Pair(5, 7), Pair(11, 13), Pair(17, 19), Pair(29, 31), Pair(41, 43), Pair(59, 61), Pair(71, 73)))
    check(findPrimePairs(15) == listOf(Pair(3, 5), Pair(5, 7), Pair(11, 13)))
    check(findPrimePairs(1) == emptyList<Pair<Int, Int>>())
}


main()