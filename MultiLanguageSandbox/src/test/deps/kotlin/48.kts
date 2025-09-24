
/**
 * Calculates the number of whole apples left after a given time has passed, 
 * assuming each apple is eaten one after another and each takes a certain amount of time to eat.
 * 
 * @param appleCount The total number of apples initially available.
 * @param timePerApple The time in minutes it takes to eat a single apple.
 * @param timePassed The time in minutes that has elapsed.
 * @return The number of whole apples remaining.
 * 
 * Example:
 * >>> remainingApples(50, 10, 200)
 *     30
 * >>> remainingApples(5, 5, 3)
 *     4
 */
fun remainingApples(appleCount: Int, timePerApple: Int, timePassed: Int): Int 
{
    // Calculate how many apples have been eaten completely
    val applesEaten = timePassed / timePerApple
    // The remaining apples is the initial count minus the eaten ones, but not less than 0
    return maxOf(0, appleCount - applesEaten)
}
fun main() {
    check(remainingApples(50, 10, 200) == 30) // Test case based on example 1
    check(remainingApples(5, 5, 3) == 4) // Test case based on example 2
    check(remainingApples(10, 0, 100) == 0) // Test for edge case where timePerApple is 0
    check(remainingApples(100, 1, 10000) == 0) // Test for case where all apples can be eaten
}


main()