


object Solution {
    /**
    * Returns the elements of the matrix in clockwise spiral order.
    *
    * @param matrix The input matrix.
    * @return The elements of the matrix in spiral order.
    *
    * Example:
    * spiralOrder(Array(Array(1,2,3),Array(4,5,6),Array(7,8,9)))
    * Output: List(1, 2, 3, 6, 9, 8, 7, 4, 5)
    */
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
  if (matrix.isEmpty || matrix(0).isEmpty) return List.empty
  
  var result = List.empty[Int]
  var top = 0
  var bottom = matrix.length - 1
  var left = 0
  var right = matrix(0).length - 1
  
  while (top <= bottom && left <= right) {
    // Traverse from left to right on top row
    for (i <- left to right) {
      result = result :+ matrix(top)(i)
    }
    top += 1
    
    // Traverse from top to bottom on right column
    for (i <- top to bottom) {
      result = result :+ matrix(i)(right)
    }
    right -= 1
    
    if (top <= bottom) {
      // Traverse from right to left on bottom row
      for (i <- right to left by -1) {
        result = result :+ matrix(bottom)(i)
      }
      bottom -= 1
    }
    
    if (left <= right) {
      // Traverse from bottom to top on left column
      for (i <- bottom to top by -1) {
        result = result :+ matrix(i)(left)
      }
      left += 1
    }
  }
  
  result
}
  def main(args: Array[String]): Unit = {
    val matrix1 = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
    val result1 = spiralOrder(matrix1)
    assert(result1 == List(1, 2, 3, 6, 9, 8, 7, 4, 5))

    val matrix2 = Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12))
    val result2 = spiralOrder(matrix2)
    assert(result2 == List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7))

    val matrix3 = Array(Array(1, 2, 3, 4, 5), Array(6, 7, 8, 9, 10), Array(11, 12, 13, 14, 15))
    val result3 = spiralOrder(matrix3)
    assert(result3 == List(1, 2, 3, 4, 5, 10, 15, 14, 13, 12, 11, 6, 7, 8, 9))

    println("All tests passed")
  }
}