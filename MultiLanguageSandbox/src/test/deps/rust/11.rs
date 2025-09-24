
/*
  Calculates the number of distinct paths from the top-left corner to the bottom-right corner of a grid,
  with the constraint that one can only move right or down and cannot enter cells where both row and column numbers are even.

  Arguments:
  - rows: The number of rows in the grid.
  - cols: The number of columns in the grid.

  Returns:
  - The number of distinct paths satisfying the given constraints.

  Example:
  - count_walking_paths(3, 4) returns 2
  - count_walking_paths(2, 2) returns 0
  - count_walking_paths(4, 5) returns 3
*/

fn count_walking_paths(rows: i32, cols: i32) -> i32 {
    let rows = rows as usize;
    let cols = cols as usize;
    
    // Create a DP table initialized to 0
    let mut dp = vec![vec![0; cols]; rows];
    
    // Check if the starting cell (0,0) is forbidden
    if rows == 0 || cols == 0 {
        return 0;
    }
    if (0 % 2 == 0) && (0 % 2 == 0) {
        return 0;
    }
    dp[0][0] = 1;
    
    // Fill the first column
    for i in 1..rows {
        if (i % 2 == 0) && (0 % 2 == 0) {
            dp[i][0] = 0;
        } else {
            dp[i][0] = dp[i-1][0];
        }
    }
    
    // Fill the first row
    for j in 1..cols {
        if (0 % 2 == 0) && (j % 2 == 0) {
            dp[0][j] = 0;
        } else {
            dp[0][j] = dp[0][j-1];
        }
    }
    
    // Fill the rest of the table
    for i in 1..rows {
        for j in 1..cols {
            if (i % 2 == 0) && (j % 2 == 0) {
                dp[i][j] = 0;
            } else {
                dp[i][j] = dp[i-1][j] + dp[i][j-1];
            }
        }
    }
    
    dp[rows-1][cols-1]
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn main() {
        assert_eq!(count_walking_paths(3, 4), 2);
        assert_eq!(count_walking_paths(2, 2), 0);
        assert_eq!(count_walking_paths(4, 5), 3);
        assert_eq!(count_walking_paths(1, 1), 1);        
    }
    

}