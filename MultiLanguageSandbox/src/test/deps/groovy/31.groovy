
class MatrixDiagonalTraversal {
    static List traverseDiagonally(ArrayList matrix) {
        """
        Traverse a given matrix in a diagonal order and return the elements in a list.
        >>> traverseDiagonally([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
        [1, 2, 4, 7, 5, 3, 6, 8, 9]
        >>> traverseDiagonally([[1, 2], [3, 4]])
        [1, 2, 3, 4]
        """

    if (matrix == null || matrix.isEmpty()) return []
    
    int rows = matrix.size()
    int cols = matrix[0].size()
    List result = []
    
    for (int sum = 0; sum < rows + cols - 1; sum++) {
        if (sum % 2 == 0) {
            // Traverse from top-right to bottom-left
            int i = Math.min(sum, rows - 1)
            int j = sum - i
            while (i >= 0 && j < cols) {
                result.add(matrix[i][j])
                i--
                j++
            }
        } else {
            // Traverse from bottom-left to top-right
            int j = Math.min(sum, cols - 1)
            int i = sum - j
            while (j >= 0 && i < rows) {
                result.add(matrix[i][j])
                i++
                j--
            }
        }
    }
    
    return result
}
}
// Test cases
def matrixDiagonalTraversal = new MatrixDiagonalTraversal()
assert matrixDiagonalTraversal.traverseDiagonally([[1, 2, 3], [4, 5, 6], [7, 8, 9]]) == [1, 2, 4, 7, 5, 3, 6, 8, 9]
assert matrixDiagonalTraversal.traverseDiagonally([[1, 2], [3, 4]]) == [1, 2, 3, 4]
assert matrixDiagonalTraversal.traverseDiagonally([[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]) == [1, 2, 5, 9, 6, 3, 4, 7, 10, 11, 8, 12]
assert matrixDiagonalTraversal.traverseDiagonally([[1, 2, 3, 4, 5]]) == [1, 2, 3, 4, 5]
assert matrixDiagonalTraversal.traverseDiagonally([[1], [2], [3], [4], [5]]) == [1, 2, 3, 4, 5]