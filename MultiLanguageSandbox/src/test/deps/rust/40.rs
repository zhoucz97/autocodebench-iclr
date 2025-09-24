
/*
  Calculates the number of different ways to pass a ball among 'n' students standing in a circle, such that after 'm' passes, the ball returns to the original student.

  Arguments:
  - n: The number of students in the circle. It is an integer where 3 <= n <= 30.
  - m: The number of times the ball is passed. It is an integer where 1 <= m <= 30.

  Returns:
  - The number of distinct passing sequences that return the ball to the starting student after 'm' passes.

  Examples:
  - count_passing_ways(3, 3) returns 2
  - count_passing_ways(4, 2) returns 4
  - count_passing_ways(5, 5) returns 6
*/
fn count_passing_ways(n: i32, m: i32) -> i32 {
    let n = n as usize;
    let m = m as usize;
    
    // Initialize DP table: dp[i][j] represents the number of ways to reach student j after i passes
    let mut dp = vec![vec![0; n]; m + 1];
    dp[0][0] = 1; // Starting at student 0 with 0 passes
    
    for pass in 1..=m {
        for current in 0..n {
            let left = (current + n - 1) % n;
            let right = (current + 1) % n;
            dp[pass][current] = dp[pass - 1][left] + dp[pass - 1][right];
        }
    }
    
    dp[m][0]
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_passing_ways(3, 3), 2);
        assert_eq!(count_passing_ways(4, 2), 2);
        assert_eq!(count_passing_ways(5, 5), 2);    
        assert_eq!(count_passing_ways(10, 3), 0);    
        assert_eq!(count_passing_ways(30, 10), 252);    
    }
    

}