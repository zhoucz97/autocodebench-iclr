

/**
    * Calculate the number of ways a person can mistakenly put letters in envelopes
    * such that each letter ends up in the wrong envelope.
    *
    * This is based on a derangement problem where each item (letter) must not be
    * in its original position (envelope).
    *
    * Parameters:
    * n - the number of letters and envelopes.
    *
    * Returns:
    * The total number of ways to wrongly place all the letters.
    *
    * Examples:
    * >>> countWrongEnvelopeCombinations(2)
    *     1
    * >>> countWrongEnvelopeCombinations(3)
    *     2
**/
fun countWrongEnvelopeCombinations(n: Int): Long 
{
    if (n == 0) return 1L
    if (n == 1) return 0L
    
    var d0 = 1L // !0
    var d1 = 0L // !1
    
    for (i in 2..n) {
        val di = (i - 1) * (d0 + d1)
        d0 = d1
        d1 = di
    }
    
    return d1
}
fun main() {
    check(countWrongEnvelopeCombinations(2) == 1L)
    check(countWrongEnvelopeCombinations(3) == 2L)
    check(countWrongEnvelopeCombinations(4) == 9L) // Additional test case for 4 letters
    check(countWrongEnvelopeCombinations(5) == 44L) // Additional test case for 5 letters
    check(countWrongEnvelopeCombinations(15) == 481066515734L) // Additional test case for 15 letters
}
main()