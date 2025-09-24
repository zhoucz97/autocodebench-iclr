
/*
  Determines if a sequence is a "Jolly Jumper". A sequence is considered a "Jolly Jumper" if the absolute differences between successive elements, when sorted, form a sequence of consecutive numbers starting from 1. For example, a sequence of [3, 1, 4, 2] is a Jolly Jumper because the absolute differences are 2, 3, 2, forming a consecutive sequence 1, 2, 3. A sequence with a single element is always a Jolly Jumper.

  Arguments:
  - sequence: A slice of integers, representing the sequence.

  Returns:
  - A boolean value indicating whether the sequence is a Jolly Jumper.

  Examples:
  - is_jolly_jumper(&[1, 4, 2, 3]) returns true.
  - is_jolly_jumper(&[1, 3]) returns false.
  - is_jolly_jumper(&[5]) returns true.
*/


fn is_jolly_jumper(sequence: &[i32]) -> bool {
    let n = sequence.len();
    
    // A single element is always a Jolly Jumper
    if n <= 1 {
        return true;
    }
    
    // Calculate absolute differences between successive elements
    let mut diffs: Vec<i32> = sequence.windows(2)
        .map(|window| (window[0] - window[1]).abs())
        .collect();
    
    // Sort the differences
    diffs.sort_unstable();
    
    // Check if the sorted differences form a consecutive sequence starting from 1
    for (i, &diff) in diffs.iter().enumerate() {
        if diff != (i + 1) as i32 {
            return false;
        }
    }
    
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_jolly_jumper() {
        assert!(is_jolly_jumper(&[3, 1, 4, 2]));  // Example from problem statement
        assert!(is_jolly_jumper(&[1, 4, 2, 3]));  // Test case 1
        assert(!is_jolly_jumper(&[1, 3]));       // Test case 2
        assert!(is_jolly_jumper(&[5]));           // Test case 3
        assert!(is_jolly_jumper(&[1, 2, 3, 4]));  // Consecutive numbers
        assert(!is_jolly_jumper(&[1, 2, 4, 6])); // Differences are 1, 2, 2 (not consecutive)
        assert!(is_jolly_jumper(&[9, 7, 8, 6, 10])); // Differences are 2, 1, 2, 4 (sorted: 1, 2, 2, 4)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(is_jolly_jumper(&[1, 4, 2, 3]), true);
        assert_eq!(is_jolly_jumper(&[1, 3]), false);
        assert_eq!(is_jolly_jumper(&[5]), true);
        assert_eq!(is_jolly_jumper(&[10, 7, 8, 9]), false);
    }
    

}