
class SpiralMatrix {
    static List spiralOrder(ArrayList matrix) {
        """
        Given an m x n matrix, return all elements of the matrix in spiral order.
        >>> spiralOrder([[1,2,3],[4,5,6],[7,8,9]])
        [1,2,3,6,9,8,7,4,5]
        >>> spiralOrder([[1,2,3,4],[5,6,7,8],[9,10,11,12]])
        [1,2,3,4,8,12,11,10,9,5,6,7]
        """

    if (matrix == null || matrix.isEmpty()) return []
    
    int top = 0, bottom = matrix.size() - 1
    int left = 0, right = matrix[0].size() - 1
    List result = []
    
    while (top <= bottom && left <= right) {
        // Traverse from left to right on top row
        for (int i = left; i <= right; i++) {
            result.add(matrix[top][i])
        }
        top++
        
        // Traverse from top to bottom on right column
        for (int i = top; i <= bottom; i++) {
            result.add(matrix[i][right])
        }
        right--
        
        if (top <= bottom) {
            // Traverse from right to left on bottom row
            for (int i = right; i >= left; i--) {
                result.add(matrix[bottom][i])
            }
            bottom--
        }
        
        if (left <= right) {
            // Traverse from bottom to top on left column
            for (int i = bottom; i >= top; i--) {
                result.add(matrix[i][left])
            }
            left++
        }
    }
    
    return result
}
}
// Test cases
def spiralMatrix = new SpiralMatrix()
assert spiralMatrix.spiralOrder([[1,2,3],[4,5,6],[7,8,9]]) == [1,2,3,6,9,8,7,4,5]
assert spiralMatrix.spiralOrder([[1,2,3,4],[5,6,7,8],[9,10,11,12]]) == [1,2,3,4,8,12,11,10,9,5,6,7]
assert spiralMatrix.spiralOrder([[1,2],[3,4]]) == [1,2,4,3]
assert spiralMatrix.spiralOrder([[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]]) == [1,2,3,4,5,10,15,14,13,12,11,6,7,8,9]
assert spiralMatrix.spiralOrder([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]) == [1,2,3,4,8,12,16,15,14,13,9,5,6,7,11,10]
assert spiralMatrix.spiralOrder([[1,2,3],[4,5,6],[7,8,9],[10,11,12]]) == [1,2,3,6,9,12,11,10,7,4,5,8]
assert spiralMatrix.spiralOrder([[1,2,3,4,5]]) == [1,2,3,4,5]
assert spiralMatrix.spiralOrder([[1],[2],[3],[4],[5]]) == [1,2,3,4,5]