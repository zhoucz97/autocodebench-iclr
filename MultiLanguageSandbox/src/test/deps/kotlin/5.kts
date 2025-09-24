

/**
    * Given a limit, find all Pythagorean triplets (a, b, c) such that a < b < c and a + b + c <= limit.
    * A Pythagorean triplet is a set of three positive integers a, b, and c, where a^2 + b^2 = c^2.
    * The function returns a list of triples (a, b, c) that satisfy the conditions.
    *
    * Example:
    * >>> findPythagoreanTriplets(30)
    * [(3, 4, 5), (5, 12, 13), (6, 8, 10)]
    *
    * Explanation: For limit=30, the function finds Pythagorean triplets where a + b + c is less than or equal to 30.
    * (3, 4, 5) is the first triplet with a^2 + b^2 = c^2 and a + b + c = 12, which is less than 30.
    * (5, 12, 13) and (6, 8, 10) are other valid triplets.
**/


fun findPythagoreanTriplets(limit: Int): List<Triple<Int, Int, Int>> 
{
    val triplets = mutableListOf<Triple<Int, Int, Int>>()
    
    for (a in 1 until limit) {
        for (b in a + 1 until limit) {
            val cSquared = a * a + b * b
            val c = Math.sqrt(cSquared.toDouble()).toInt()
            
            if (c * c == cSquared && c > b && a + b + c <= limit) {
                triplets.add(Triple(a, b, c))
            }
        }
    }
    
    return triplets
}
fun main() {
    val result1 = findPythagoreanTriplets(20)
    val expected1 = listOf(Triple(3, 4, 5))
    check(result1 == expected1) // Should print true

    
    val result2 = findPythagoreanTriplets(30)
    val expected2 = listOf(Triple(3, 4, 5), Triple(5, 12, 13), Triple(6, 8, 10))
    check(result2 == expected2) // Should print true
 
}


main()