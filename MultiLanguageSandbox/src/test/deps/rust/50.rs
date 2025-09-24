
use std::collections::HashSet;

/*
  Counts the number of students who have enrolled in both Course A and Course B.

  Arguments:
  - n: The number of students enrolled in Course A.
  - m: The number of students enrolled in Course B.
  - a: A vector containing the IDs of students enrolled in Course A.
  - b: A vector containing the IDs of students enrolled in Course B.

  Returns:
  - The number of students who are enrolled in both courses.

  Example:
  - count_students_enrolled_in_both(5, 5, vec![1, 2, 3, 4, 5], vec![1, 3, 4, 5, 6]) returns 4
  - count_students_enrolled_in_both(3, 2, vec![7, 8, 9], vec![10, 11]) returns 0
*/
fn count_students_enrolled_in_both(n: usize, m: usize, a: Vec<usize>, b: Vec<usize>) -> usize {
    // Create a HashSet from the first vector for O(1) lookups
    let set_a: HashSet<_> = a.into_iter().collect();
    
    // Count how many elements in b are present in set_a
    b.into_iter().filter(|&id| set_a.contains(&id)).count()
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_students_enrolled_in_both(5, 5, vec![1, 2, 3, 4, 5], vec![1, 3, 4, 5, 6]), 4);
        assert_eq!(count_students_enrolled_in_both(3, 2, vec![7, 8, 9], vec![10, 11]), 0);
        assert_eq!(count_students_enrolled_in_both(4, 4, vec![1, 3, 5, 7], vec![2, 4, 1, 3]), 2);
    
    }

}