
/**
 * Compares the area of a square with side length 'a' to the area of a rectangle with sides 'b' and 'c'.
 * Returns "Alice" if the area of the square is larger, and "Bob" if the area of the rectangle is larger.
 * 
 * Example:
 * >>> compareAreas(5, 4, 6)
 *     "Alice"
 * >>> compareAreas(7, 5, 10)
 *     "Bob"
 */
fun compareAreas(a: Int, b: Int, c: Int): String 
{
    val squareArea = a * a
    val rectangleArea = b * c
    
    return if (squareArea > rectangleArea) {
        "Alice"
    } else {
        "Bob"
    }
}
fun main() {
    check(compareAreas(5, 4, 6) == "Alice")
    check(compareAreas(7, 5, 10) == "Bob")
    // Additional test cases for edge cases
    check(compareAreas(1, 1, 1) == "Bob") // Edge case where all sides are equal
    check(compareAreas(10, 5, 20) == "Bob") // Test case where rectangle is clearly larger
}


main()