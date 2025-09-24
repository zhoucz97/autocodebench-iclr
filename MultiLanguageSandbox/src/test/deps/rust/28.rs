
use std::collections::HashSet;

/*
  Removes duplicate numbers from a given vector and then sorts the remaining numbers in ascending order.

  Arguments:
  - numbers: A vector of integers ranging from 1 to 1000, with a length not exceeding 100.

  Returns:
  - A vector containing the unique elements of the original vector, sorted in ascending order.

  Examples:
  - dedup_and_sort(vec![2, 3, 2, 5, 3]) returns vec![2, 3, 5]
  - dedup_and_sort(vec![7, 5, 7, 5]) returns vec![5, 7]
  - dedup_and_sort(vec![1, 2, 3, 4, 5]) returns vec![1, 2, 3, 4, 5]
*/

fn dedup_and_sort(mut numbers: Vec<i32>) -> Vec<i32> {
    // Remove duplicates by converting to a HashSet (which automatically deduplicates)
    let unique_numbers: HashSet<_> = numbers.drain(..).collect();
    
    // Convert back to a Vec and sort it
    let mut result: Vec<i32> = unique_numbers.into_iter().collect();
    result.sort();
    
    result
}

// Example usage:


#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(dedup_and_sort(vec![10, 20, 20, 10, 30]), vec![10, 20, 30]);
        assert_eq!(dedup_and_sort(vec![5, 4, 3, 2, 1]), vec![1, 2, 3, 4, 5]);
        assert_eq!(dedup_and_sort(vec![1, 1, 1, 1, 1]), vec![1]);
        assert_eq!(dedup_and_sort(vec![50]), vec![50]);
    }
    

}