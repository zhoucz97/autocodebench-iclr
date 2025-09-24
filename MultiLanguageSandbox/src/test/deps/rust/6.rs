
/*
  Determines the length of the longest contiguous sequence of characters from 'target' that is contained in 'source'.
  The function does not require the characters to be consecutively positioned in 'source', but they must retain their order from 'target'.

  Arguments:
  - source: A string consisting of uppercase English letters.
  - target: A string consisting of uppercase English letters.

  Returns:
  - The length of the longest contiguous sequence of characters from 'target' that is contained in 'source'.

  Example:
  - longest_contained_sequence("ABCDEABCD", "AABZ") returns 3
  - longest_contained_sequence("XYZ", "XYZ") returns 3
  - longest_contained_sequence("ABCDEF", "FED") returns 0
*/


fn longest_contained_sequence(source: &str, target: &str) -> usize {
    let source_chars: Vec<char> = source.chars().collect();
    let target_chars: Vec<char> = target.chars().collect();
    let m = source_chars.len();
    let n = target_chars.len();
    
    // Create a DP table initialized to 0
    let mut dp = vec![vec![0; n + 1]; m + 1];
    let mut max_len = 0;
    
    for i in 1..=m {
        for j in 1..=n {
            if source_chars[i - 1] == target_chars[j - 1] {
                dp[i][j] = dp[i - 1][j - 1] + 1;
                if dp[i][j] > max_len {
                    max_len = dp[i][j];
                }
            } else {
                dp[i][j] = dp[i - 1][j];
            }
        }
    }
    
    max_len
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(longest_contained_sequence("ABCDEABCD", "AABZ"), 3);
        assert_eq!(longest_contained_sequence("XYZ", "XYZ"), 3);
        assert_eq!(longest_contained_sequence("ABCDEF", "FED"), 1);
        assert_eq!(longest_contained_sequence("ABCD", "BCD"), 3);
    }
    

}