
/*
  Calculates the number of paths a pawn can take to move from the origin (0, 0) to a given point (n, m) on a chessboard,
  avoiding the control points of a stationary horse located at (horse_x, horse_y).

  The pawn can only move either one step to the right or one step down at a time. The horse's control points include its 
  current position and all the points it can reach in one move. The pawn must avoid these control points.

  Arguments:
  - n: x-coordinate of the destination point, an integer not exceeding 15.
  - m: y-coordinate of the destination point, an integer not exceeding 15.
  - horse_x: x-coordinate of the horse's position, an integer.
  - horse_y: y-coordinate of the horse's position, an integer.

  Returns:
  - The total number of valid paths from (0, 0) to (n, m) avoiding the horse's control points.

  Examples:
  - count_paths(6, 6, 3, 3) returns 6
  - count_paths(5, 5, 1, 1) returns 12
  - count_paths(7, 7, 2, 2) returns 28
*/


fn count_paths(n: usize, m: usize, horse_x: usize, horse_y: usize) -> usize {
    // Create a set of forbidden positions including the horse's position and its attack positions
    let mut forbidden = std::collections::HashSet::new();
    forbidden.insert((horse_x, horse_y));
    
    // Add all possible knight moves from (horse_x, horse_y)
    let moves = [
        (2, 1), (1, 2), (-1, 2), (-2, 1),
        (-2, -1), (-1, -2), (1, -2), (2, -1)
    ];
    
    for (dx, dy) in moves.iter() {
        let x = horse_x as isize + dx;
        let y = horse_y as isize + dy;
        if x >= 0 && y >= 0 {
            forbidden.insert((x as usize, y as usize));
        }
    }
    
    // Initialize DP table
    let mut dp = vec![vec![0; m + 1]; n + 1];
    
    // Base case: starting point (0, 0)
    if !forbidden.contains(&(0, 0)) {
        dp[0][0] = 1;
    }
    
    // Fill the first row: can only come from left
    for j in 1..=m {
        if !forbidden.contains(&(0, j)) {
            dp[0][j] = dp[0][j - 1];
        }
    }
    
    // Fill the first column: can only come from above
    for i in 1..=n {
        if !forbidden.contains(&(i, 0)) {
            dp[i][0] = dp[i - 1][0];
        }
    }
    
    // Fill the rest of the table
    for i in 1..=n {
        for j in 1..=m {
            if !forbidden.contains(&(i, j)) {
                dp[i][j] = dp[i - 1][j] + dp[i][j - 1];
            }
        }
    }
    
    dp[n][m]
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_paths(6, 6, 3, 3), 6);
        assert_eq!(count_paths(5, 5, 1, 1), 10);
        assert_eq!(count_paths(7, 7, 2, 2), 0);
    }
    

}