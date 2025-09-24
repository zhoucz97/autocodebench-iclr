
/*
    Calculates the maximum sum of numbers along a path from the top to the bottom of a given numerical triangle.

    The path moves down to either the nearest left or right number on the next row. The difference between the number of moves to the left and to the right must not exceed one.

    Arguments:
    - triangle: A vector of vectors of i32, representing the rows of the numerical triangle. Each inner vector represents a row in the triangle.

    Returns:
    - The maximum sum achievable on any valid path from the top to the bottom of the triangle.

    Examples:
    - max_triangle_path_sum(vec![vec![7], vec![3, 8], vec![8, 1, 0], vec![2, 7, 4, 4], vec![4, 5, 2, 6, 5]]) returns 27
    - max_triangle_path_sum(vec![vec![3], vec![3, 1], vec![5, 2, 3]]) returns 11
*/

fn max_triangle_path_sum(triangle: Vec<Vec<i32>>) -> i32 {
    let n = triangle.len();
    if n == 0 {
        return 0;
    }
    
    let mut dp = vec![vec![0; triangle[i].len()]; n];
    dp[0][0] = triangle[0][0];
    
    for i in 1..n {
        for j in 0..triangle[i].len() {
            let mut max_prev = 0;
            // Check the possible parents: (i-1, j-1) and (i-1, j)
            if j > 0 && j - 1 < triangle[i-1].len() {
                max_prev = dp[i-1][j-1].max(max_prev);
            }
            if j < triangle[i-1].len() {
                max_prev = dp[i-1][j].max(max_prev);
            }
            dp[i][j] = max_prev + triangle[i][j];
        }
    }
    
    *dp[n-1].iter().max().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_triangle_path_sum(vec![vec![7], vec![3, 8], vec![8, 1, 0], vec![2, 7, 4, 4], vec![4, 5, 2, 6, 5]]), 27);
        assert_eq!(max_triangle_path_sum(vec![vec![3], vec![3, 1], vec![5, 2, 3]]), 8);      
    }
    

}