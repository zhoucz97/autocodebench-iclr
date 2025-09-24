
/**
 * Finds the element with the largest absolute value in a 3x4 matrix and returns its absolute value and indices.
 * If multiple elements have the same absolute value, the one with the smallest row index is returned,
 * and if there is still a tie, the one with the smallest column index is chosen.
 *
 * Example:
 * >>> findMaxAbsElement(arrayOf(arrayOf(-1, 2, 3, -4), arrayOf(5, -6, 7, 8), arrayOf(-9, 10, 11, 12)))
 *     Triple(12, 2, 3) // Absolute value 12 at row 2, column 3
 * >>> findMaxAbsElement(arrayOf(arrayOf(-4, -4, 3, 2), arrayOf(1, 0, -4, -4), arrayOf(2, 3, -4, 4)))
 *     Triple(4, 0, 0) // Absolute value 4 at row 0, column 0 (first occurrence)
 */
fun findMaxAbsElement(matrix: Array<Array<Int>>): Triple<Int, Int, Int> 
{
    var maxAbs = Int.MIN_VALUE
    var maxRow = 0
    var maxCol = 0
    
    for (i in matrix.indices) {
        for (j in matrix[i].indices) {
            val currentAbs = Math.abs(matrix[i][j])
            if (currentAbs > maxAbs || 
                (currentAbs == maxAbs && (i < maxRow || (i == maxRow && j < maxCol)))) {
                maxAbs = currentAbs
                maxRow = i
                maxCol = j
            }
        }
    }
    
    return Triple(maxAbs, maxRow, maxCol)
}
fun main() {
    check(findMaxAbsElement(arrayOf(arrayOf(-1, 2, 3, -4), arrayOf(5, -6, 7, 8), arrayOf(-9, 10, 11, 12))) == Triple(12, 2, 3))
    check(findMaxAbsElement(arrayOf(arrayOf(-4, -4, 3, 2), arrayOf(1, 0, -4, -4), arrayOf(2, 3, -4, 4))) == Triple(4, 0, 0))
    check(findMaxAbsElement(arrayOf(arrayOf(1, -2, 3, -4), arrayOf(0, 6, -7, 8), arrayOf(9, -10, -11, -12))) == Triple(12, 2, 3))
    check(findMaxAbsElement(arrayOf(arrayOf(4, 4, -3, -2), arrayOf(-1, 0, 4, 4), arrayOf(-2, -3, 4, -4))) == Triple(4, 0, 0))
    check(findMaxAbsElement(arrayOf(arrayOf(0, 0, 0, 0), arrayOf(0, 0, 0, 0), arrayOf(0, 0, 0, 0))) == Triple(0, 0, 0))
}
main()