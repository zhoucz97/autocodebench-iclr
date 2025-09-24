
/**
    * Returns the top ten highest scores from a given list of scores, sorted in descending order.
    * If there are fewer than ten scores in the list, all scores are returned.
    * Example:
    >>> topTenScores(listOf(12.5, 15.0, 9.5, 18.0, 22.0, 7.0, 14.5, 16.5, 17.5, 19.5, 20.5, 21.5))
        [22.0, 21.5, 20.5, 19.5, 18.0, 17.5, 16.5, 15.0, 14.5, 12.5]
    >>> topTenScores(listOf(5.0, 3.0, 4.0))
        [5.0, 4.0, 3.0]
**/
fun topTenScores(scores: List<Double>): List<Double> 
{
    return scores.sortedDescending().take(10)
}
fun main(){
    check(topTenScores(listOf(12.5, 15.0, 9.5, 18.0, 22.0, 7.0, 14.5, 16.5, 17.5, 19.5, 20.5, 21.5)) == listOf(22.0, 21.5, 20.5, 19.5, 18.0, 17.5, 16.5, 15.0, 14.5, 12.5))
    check(topTenScores(listOf(5.0, 3.0, 4.0)) == listOf(5.0, 4.0, 3.0))
    check(topTenScores(listOf(8.0, 7.5, 9.0, 10.0)) == listOf(10.0, 9.0, 8.0, 7.5))
    check(topTenScores(listOf(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0)) == listOf(12.0, 11.0, 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0))
    check(topTenScores(listOf(32.0, 31.5, 33.0, 30.0)) == listOf(33.0, 32.0, 31.5, 30.0))
}

main()