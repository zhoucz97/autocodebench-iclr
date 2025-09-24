

# Transposes a given n x m matrix.
# The function takes a single argument, matrix, which is an array of arrays representing the n x m matrix.
# The matrix has n rows and m columns, where 1 ≤ n ≤ 20 and 1 ≤ m ≤ 20. Each element in the matrix is an integer.
# The function returns a new matrix which is the transpose of the input matrix.
#
# Example:
# >>> transpose_matrix([[34, 76, -54, 7], [-4, 5, 23, 9]])
# [[34, -4], [76, 5], [-54, 23], [7, 9]]

def transpose_matrix(matrix)
  # Get the number of rows and columns in the original matrix
  rows = matrix.size
  cols = matrix[0].size

  # Initialize the transposed matrix with swapped dimensions
  transposed = Array.new(cols) { Array.new(rows) }

  # Fill the transposed matrix by swapping rows and columns
  (0...rows).each do |i|
    (0...cols).each do |j|
      transposed[j][i] = matrix[i][j]
    end
  end

  transposed
end

raise 'Test failed' unless transpose_matrix([[34, 76, -54, 7], [-4, 5, 23, 9]]) == [[34, -4], [76, 5], [-54, 23], [7, 9]]
raise 'Test failed' unless transpose_matrix([[1, 2, 3], [4, 5, 6]]) == [[1, 4], [2, 5], [3, 6]]
raise 'Test failed' unless transpose_matrix([[10]]) == [[10]]
raise 'Test failed' unless transpose_matrix([]) == []
raise 'Test failed' unless transpose_matrix([[1, 2], [3, 4], [5, 6]]) == [[1, 3, 5], [2, 4, 6]]


puts 'All tests passed!'