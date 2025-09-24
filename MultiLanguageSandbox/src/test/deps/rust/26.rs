

/*
  Determines whether the given sequence of numbers forms an arithmetic sequence.

  An arithmetic sequence is a sequence of numbers such that the difference between consecutive numbers is constant.

  Arguments:
  - sequence: An array slice of integers representing the sequence to be checked.

  Returns:
  - A boolean value indicating whether the sequence forms an arithmetic sequence (`true`) or not (`false`).

  Examples:
  - is_arithmetic_sequence(&[5, 7, 9, 11]) returns true (as the difference is 2)
  - is_arithmetic_sequence(&[1, 2, 4, 8]) returns false (as the difference is not constant)
  - is_arithmetic_sequence(&[10, 10, 10]) returns true (as the difference is 0)
*/


fn is_arithmetic_sequence(sequence: &[i32]) -> bool {
    if sequence.len() <= 2 {
        return true;
    }
    
    let diff = sequence[1] - sequence[0];
    for i in 2..sequence.len() {
        if sequence[i] - sequence[i - 1] != diff {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(is_arithmetic_sequence(&[5, 7, 9, 11]), true);  // True, as 2 is the constant difference
        assert_eq!(is_arithmetic_sequence(&[1, 2, 4, 8]), false);  // False, as difference changes
        assert_eq!(is_arithmetic_sequence(&[10, 10, 10]), true);   // True, as 0 is the constant difference
        assert_eq!(is_arithmetic_sequence(&[3, 6, 9, 12, 16]), false); // False, last difference is not 3
    }
}