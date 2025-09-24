
/*
  Calculates the sum of scores for all non-empty substrings of a given string, where the score of a substring is defined as the count of characters that appear exactly once in it.

  Arguments:
  - s: A string consisting of lowercase English letters.

  Returns:
  - An integer representing the total sum of scores for all non-empty substrings.

  Example:
  - sum_of_unique_chars_scores("abc") returns 10 (Scores: a=1, b=1, c=1, ab=2, bc=2, abc=3)
  - sum_of_unique_chars_scores("aab") returns 4 (Scores: a=0, a=0, b=1, aa=0, ab=1, aab=1)
*/

fn sum_of_unique_chars_scores(s: &str) -> i32 {
    let chars: Vec<char> = s.chars().collect();
    let n = chars.len();
    let mut total = 0;

    for i in 0..n {
        let mut freq = [0; 26];
        for j in i..n {
            let c = chars[j] as usize - 'a' as usize;
            freq[c] += 1;
            let mut score = 0;
            for &count in &freq {
                if count == 1 {
                    score += 1;
                }
            }
            total += score;
        }
    }

    total
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(sum_of_unique_chars_scores("abc"), 10); // a=1, b=1, c=1, ab=2, bc=2, abc=3
        assert_eq!(sum_of_unique_chars_scores("aab"), 6);  // a=1, a=1, b=1, aa=0, ab=2, aab=1
        assert_eq!(sum_of_unique_chars_scores("ababc"), 21);
    }


}