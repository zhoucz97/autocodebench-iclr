
/**
    * Check if in given list of numbers, any two numbers are closer to each other than
    * given threshold.
*/
fun hasCloseElements(numbers: List<Double>, threshold: Double): Boolean
{
    // Sort the list to easily find the closest pairs
    val sortedNumbers = numbers.sorted()
    
    // Iterate through the sorted list and check adjacent elements
    for (i in 0 until sortedNumbers.size - 1) {
        val current = sortedNumbers[i]
        val next = sortedNumbers[i + 1]
        
        // Check if the difference between adjacent elements is less than the threshold
        if (next - current < threshold) {
            return true
        }
    }
    
    // If no such pair is found, return false
    return false
}
// Test cases
fun main() {
    println("start")
    assert(hasCloseElements(listOf(1.0, 2.0, 3.9, 4.0, 5.0, 2.2), 0.3) == true)
    assert(hasCloseElements(listOf(1.0, 2.0, 3.9, 4.0, 5.0, 2.2), 0.05) == false)
    assert(hasCloseElements(listOf(1.0, 2.0, 5.9, 4.0, 5.0), 0.95) == true)
    assert(hasCloseElements(listOf(1.0, 2.0, 5.9, 4.0, 5.0), 0.8) == false)
    assert(hasCloseElements(listOf(1.0, 2.0, 3.0, 4.0, 5.0, 2.0), 0.1) == true)
    assert(hasCloseElements(listOf(1.1, 2.2, 3.1, 4.1, 5.1), 1.0) == true)
    assert(hasCloseElements(listOf(1.1, 2.2, 3.1, 4.1, 5.1), 0.5) == false)

}