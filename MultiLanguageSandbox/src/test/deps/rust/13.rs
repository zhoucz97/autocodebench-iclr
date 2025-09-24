

/*
  Converts a given sequence of numbers into the lexicographically largest possible string of uppercase English letters, where each letter corresponds to its position in the alphabet (A=1, B=2, ..., Z=26).

  The function interprets the sequence of numbers as either a single-digit or double-digit number (representing a letter) to maximize the lexicographic order of the resulting string.

  Arguments:
  - encoded_sequence: A string consisting of digits representing the encoded letter positions.

  Returns:
  - A string of uppercase English letters that represents the lexicographically largest possible interpretation of the encoded sequence.

  Examples:
  - max_lexicographic_string("123242526") returns "LCXYZ"
  - max_lexicographic_string("231819") returns "WRS"
  - max_lexicographic_string("102") returns "JB"
*/


fn max_lexicographic_string(encoded_sequence: &str) -> String {
    let chars: Vec<char> = encoded_sequence.chars().collect();
    let n = chars.len();
    if n == 0 {
        return String::new();
    }

    // dp[i] will store the best string for the first i characters
    let mut dp = vec![String::new(); n + 1];
    dp[0] = String::new();

    for i in 1..=n {
        // Option 1: take the current digit as a single digit (1-9)
        if i >= 1 {
            let single_digit = chars[i - 1].to_digit(10).unwrap();
            if single_digit >= 1 && single_digit <= 9 {
                let c = (b'A' + (single_digit - 1) as u8) as char;
                let candidate = format!("{}{}", &dp[i - 1], c);
                if dp[i].len() == 0 || candidate > dp[i] {
                    dp[i] = candidate;
                }
            }
        }

        // Option 2: take the current and previous digit as a double digit (10-26)
        if i >= 2 {
            let double_digit_str: String = chars[i - 2..i].iter().collect();
            let double_digit = double_digit_str.parse::<u32>().unwrap();
            if double_digit >= 10 && double_digit <= 26 {
                let c = (b'A' + (double_digit - 1) as u8) as char;
                let candidate = format!("{}{}", &dp[i - 2], c);
                if dp[i].len() == 0 || candidate > dp[i] {
                    dp[i] = candidate;
                }
            }
        }
    }

    dp[n].clone()
}

   
#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_lexicographic_string("123242526"), "LCXYZ");
        assert_eq!(max_lexicographic_string("231819"), "WRS");
        assert_eq!(max_lexicographic_string("102"), "JB");
        assert_eq!(max_lexicographic_string("26"), "Z");   
    }
    

}