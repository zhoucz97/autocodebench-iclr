
/*
  Calculates the maximum number of missiles that can be intercepted by the system, given their heights in the order they are detected.
  The interception system can intercept any height for the first missile, but subsequent interceptions must be at heights less than or equal to the previous interception.

  Arguments:
  - heights: A slice of integers representing the heights of incoming missiles.

  Returns:
  - An integer representing the maximum number of missiles that can be intercepted.

  Example:
  - max_missiles_intercepted(&[300, 250, 275, 252, 200, 138, 245]) returns 5
  - max_missiles_intercepted(&[500, 400, 300, 200, 100]) returns 5
  - max_missiles_intercepted(&[100, 200, 300, 400, 500]) returns 1
*/

fn max_missiles_intercepted(heights: &[i32]) -> i32 {
    if heights.is_empty() {
        return 0;
    }
    
    let n = heights.len();
    let mut dp = vec![1; n];
    
    for i in 1..n {
        for j in 0..i {
            if heights[i] <= heights[j] {
                dp[i] = dp[i].max(dp[j] + 1);
            }
        }
    }
    
    *dp.iter().max().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_missiles_intercepted(&[300, 250, 275, 252, 200, 138, 245]), 5);
        assert_eq!(max_missiles_intercepted(&[500, 400, 300, 200, 100]), 5);
        assert_eq!(max_missiles_intercepted(&[100, 200, 300, 400, 500]), 1);
    }
    

}