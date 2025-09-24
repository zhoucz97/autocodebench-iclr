
/**
    * Adds two times together. Each time is represented as a Triple of hours, minutes, and seconds.
    * Hours, minutes, and seconds are normalized to ensure minutes and seconds are within 0 to 59.
    * 
    * Example:
    * >>> addTimes(Triple(1, 2, 3), Triple(4, 5, 6))
    *     Triple(5, 7, 9)
    * >>> addTimes(Triple(23, 59, 59), Triple(0, 0, 1))
    *     Triple(24, 0, 0)
**/
fun addTimes(timeA: Triple<Int, Int, Int>, timeB: Triple<Int, Int, Int>): Triple<Int, Int, Int> 
{
    val totalSeconds = timeA.third + timeB.third
    val carryMinutes = totalSeconds / 60
    val normalizedSeconds = totalSeconds % 60
    
    val totalMinutes = timeA.second + timeB.second + carryMinutes
    val carryHours = totalMinutes / 60
    val normalizedMinutes = totalMinutes % 60
    
    val totalHours = timeA.first + timeB.first + carryHours
    
    return Triple(totalHours, normalizedMinutes, normalizedSeconds)
}
fun main(){


check(addTimes(Triple(1, 2, 3), Triple(4, 5, 6)) == Triple(5, 7, 9))
check(addTimes(Triple(23, 59, 59), Triple(0, 0, 1)) == Triple(24, 0, 0))
check(addTimes(Triple(10, 30, 30), Triple(2, 35, 40)) == Triple(13, 6, 10))
check(addTimes(Triple(0, 0, 0), Triple(0, 0, 0)) == Triple(0, 0, 0))

}

main()