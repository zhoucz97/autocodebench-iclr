


/**
  * Generates a Pascal's Triangle up to the specified number of rows.
  * In Pascal's Triangle, each number is the sum of the two numbers directly above it.
  * The function returns a list of lists, where each inner list represents a row in the triangle.
  *
  * Example:
  * >>> generatePascalsTriangle(3)
  *     [[1], [1, 1], [1, 2, 1]]
  * >>> generatePascalsTriangle(5)
  *     [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1]]
  **/


fun generatePascalsTriangle(rows: Int): List<List<Int>> 
{
    if (rows <= 0) return emptyList()
    
    val triangle = mutableListOf<List<Int>>()
    triangle.add(listOf(1)) // First row
    
    for (i in 1 until rows) {
        val previousRow = triangle[i - 1]
        val currentRow = mutableListOf<Int>()
        
        currentRow.add(1) // First element is always 1
        
        for (j in 1 until i) {
            currentRow.add(previousRow[j - 1] + previousRow[j])
        }
        
        currentRow.add(1) // Last element is always 1
        triangle.add(currentRow)
    }
    
    return triangle
}
fun main() {
    check(generatePascalsTriangle(1) == listOf(listOf(1)))
    check(generatePascalsTriangle(2) == listOf(listOf(1), listOf(1, 1)))
    check(generatePascalsTriangle(3) == listOf(listOf(1), listOf(1, 1), listOf(1, 2, 1)))
    check(generatePascalsTriangle(4) == listOf(listOf(1), listOf(1, 1), listOf(1, 2, 1), listOf(1, 3, 3, 1)))
    check(generatePascalsTriangle(5) == listOf(listOf(1), listOf(1, 1), listOf(1, 2, 1), listOf(1, 3, 3, 1), listOf(1, 4, 6, 4, 1)))
    check(generatePascalsTriangle(6) == listOf(listOf(1), listOf(1, 1), listOf(1, 2, 1), listOf(1, 3, 3, 1), listOf(1, 4, 6, 4, 1), listOf(1, 5, 10, 10, 5, 1)))

}

main()