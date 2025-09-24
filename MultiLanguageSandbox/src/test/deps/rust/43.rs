
/*
  Calculates the length of the longest non-decreasing subsequence in a given sequence of numbers.
  
  The function takes a vector of integers and returns the length of the longest subsequence where each element is not less than the previous one.

  Arguments:
  - sequence: A vector of integers.

  Returns:
  - The length of the longest non-decreasing subsequence in the given sequence.

  Examples:
  - longest_non_decreasing_subsequence_length(vec![1, 2, 3]) returns 3
  - longest_non_decreasing_subsequence_length(vec![4, 10, 4, 3, 8, 9]) returns 3
  - longest_non_decreasing_subsequence_length(vec![6, 1, 5, 2, 3, 4]) returns 4
*/

fn longest_non_decreasing_subsequence_length(sequence: Vec<i32>) -> usize {
    if sequence.is_empty() {
        return 0;
    }
    
    let n = sequence.len();
    let mut dp = vec![1; n];
    
    for i in 1..n {
        for j in 0..i {
            if sequence[i] >= sequence[j] {
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
        assert_eq!(longest_non_decreasing_subsequence_length(vec![1, 2, 3]), 3);
        assert_eq!(longest_non_decreasing_subsequence_length(vec![4, 10, 4, 3, 8, 9]), 4);
        assert_eq!(longest_non_decreasing_subsequence_length(vec![6, 1, 5, 2, 3, 4]), 4);
        assert_eq!(longest_non_decreasing_subsequence_length(vec![]), 0);
        assert_eq!(longest_non_decreasing_subsequence_length(vec![10]), 1);        
    }
    

}