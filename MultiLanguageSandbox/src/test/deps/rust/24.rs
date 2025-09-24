
/*
  Computes the transpose of a given square matrix.

  Arguments:
  - matrix: A square matrix represented as a vector of vectors of i32.

  Returns:
  - A new matrix which is the transpose of the input matrix. In the transposed matrix, the rows and columns of the original matrix are switched, so that element at (i, j) in the original matrix moves to position (j, i) in the transposed matrix.

  Example:
  - transpose_matrix(vec![vec![1, 2], vec![3, 4]]) returns vec![vec![1, 3], vec![2, 4]]
*/

fn transpose_matrix(matrix: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    let n = matrix.len();
    let mut transposed = vec![vec![0; n]; n];
    
    for i in 0..n {
        for j in 0..n {
            transposed[j][i] = matrix[i][j];
        }
    }
    
    transposed
}

// Example usage:


#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(
            transpose_matrix(vec![vec![1, 2], vec![3, 4]]),
            vec![vec![1, 3], vec![2, 4]]
        );
        assert_eq!(
            transpose_matrix(vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]]),
            vec![vec![1, 4, 7], vec![2, 5, 8], vec![3, 6, 9]]
        );
        assert_eq!(
            transpose_matrix(vec![vec![1, 2], vec![1, 2]]),
            vec![vec![1, 1], vec![2, 2]]
        );
    }
    

}