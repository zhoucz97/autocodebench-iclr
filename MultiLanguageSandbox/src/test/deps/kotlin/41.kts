
/**
 * Counts the number of pairs of students who are considered "equal competitors".
 * Two students are considered equal competitors if the difference in their scores for each subject (Math, English, and Chinese)
 * is not greater than 5, and the difference in their total scores is not greater than 10.
 * 
 * @param scores A list of triples, each representing the Math, English, and Chinese scores of a student.
 * @return The number of pairs of students who are considered equal competitors.
 * 
 * Example:
 * >>> countEqualCompetitors(listOf(Triple(90, 90, 90), Triple(85, 95, 90), Triple(80, 100, 91)))
 *     2
 */

fun countEqualCompetitors(scores: List<Triple<Int, Int, Int>>): Int 
{
    var count = 0
    val n = scores.size
    
    for (i in 0 until n) {
        for (j in i + 1 until n) {
            val (math1, english1, chinese1) = scores[i]
            val (math2, english2, chinese2) = scores[j]
            
            // Check if the difference in each subject score is <= 5
            val mathDiff = Math.abs(math1 - math2)
            val englishDiff = Math.abs(english1 - english2)
            val chineseDiff = Math.abs(chinese1 - chinese2)
            
            if (mathDiff <= 5 && englishDiff <= 5 && chineseDiff <= 5) {
                // Calculate total scores
                val total1 = math1 + english1 + chinese1
                val total2 = math2 + english2 + chinese2
                val totalDiff = Math.abs(total1 - total2)
                
                // Check if total score difference is <= 10
                if (totalDiff <= 10) {
                    count++
                }
            }
        }
    }
    
    return count
}
fun main() {
    check(countEqualCompetitors(listOf(Triple(90, 90, 90), Triple(85, 95, 90), Triple(80, 100, 91))) == 2)
    check(countEqualCompetitors(listOf(Triple(100, 100, 100), Triple(92, 93, 95), Triple(101, 102, 103))) == 1)
    check(countEqualCompetitors(listOf(Triple(90, 91, 92), Triple(86, 87, 88), Triple(93, 94, 95))) == 1)
    check(countEqualCompetitors(listOf(Triple(100, 100, 100), Triple(105, 105, 105), Triple(100, 100, 100))) == 1)
}


main()