
use std::collections::HashSet;

/*
  Finds the length of the longest consecutive sequence in the given array of numbers.
  A consecutive sequence is defined as a sequence of integers where each number follows the previous one by exactly one.
  This function does not require the input array to be sorted.

  Arguments:
  - numbers: A slice of integers representing the sequence.

  Returns:
  - The length of the longest consecutive sequence found in the array.

  Example:
  - longest_consecutive_sequence(&[1, 9, 3, 10, 4, 20, 2]) returns 4 (because the longest consecutive sequence is 1, 2, 3, 4)
  - longest_consecutive_sequence(&[8, 4, 2, 1, 3, 5]) returns 5 (because the sequence is 1, 2, 3, 4, 5)
  - longest_consecutive_sequence(&[100, 4, 200, 1, 3, 2]) returns 4 (because the sequence is 1, 2, 3, 4)
*/
use std::collections::HashSet;

fn longest_consecutive_sequence(numbers: &[i32]) -> usize {
    let num_set: HashSet<_> = numbers.iter().cloned().collect();
    let mut max_length = 0;

    for &num in numbers {
        // Only check if it's the start of a sequence
        if !num_set.contains(&(num - 1)) {
            let mut current_num = num;
            let mut current_length = 1;

            while num_set.contains(&(current_num + 1)) {
                current_num += 1;
                current_length += 1;
            }

            max_length = max_length.max(current_length);
        }
    }

    max_length
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_longest_consecutive_sequence() {
        assert_eq!(longest_consecutive_sequence(&[1, 9, 3, 10, 4, 20, 2]), 4);
        assert_eq!(longest_consecutive_sequence(&[8, 4, 2, 1, 3, 5]), 5);
        assert_eq!(longest_consecutive_sequence(&[100, 4, 200, 1, 3, 2]), 4);
        assert_eq!(longest_consecutive_sequence(&[]), 0);
        assert_eq!(longest_consecutive_sequence(&[1]), 1);
        assert_eq!(longest_consecutive_sequence(&[1, 2, 0, 1]), 3);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(longest_consecutive_sequence(&[1, 9, 3, 10, 4, 20, 2]), 4);
        assert_eq!(longest_consecutive_sequence(&[8, 4, 2, 1, 3, 5]), 5);
        assert_eq!(longest_consecutive_sequence(&[100, 4, 200, 1, 3, 2]), 4);
        assert_eq!(longest_consecutive_sequence(&[6, 6, 2, 3, 1, 4, 5, 7]), 7);
    }
    

}