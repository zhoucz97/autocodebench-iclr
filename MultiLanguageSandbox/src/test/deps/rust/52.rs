
use std::collections::HashMap;

/*
  Finds the missing and duplicate ID numbers from a series of ID sequences.
  Each sequence of IDs is represented as a vector within a larger vector, simulating the input format described.
  The IDs are assumed to be part of a continuous range but with one missing ID and one duplicate ID.
  The function identifies and returns both the missing ID (that has been skipped) and the duplicate ID (that appears more than once).

  Arguments:
  - data: A vector of vectors, where each inner vector contains a sequence of ID numbers.

  Returns:
  - A tuple containing two usize values: the first is the missing ID, and the second is the duplicate ID.

  Example:
  - find_missing_and_duplicate_ids(vec![vec![5, 6, 8, 11, 9], vec![10, 12, 9]]) returns (7, 9)
*/
fn find_missing_and_duplicate_ids(data: Vec<Vec<usize>>) -> (usize, usize) {
    let mut all_ids = Vec::new();
    
    // Flatten all the vectors into one vector
    for ids in data {
        all_ids.extend(ids);
    }
    
    if all_ids.is_empty() {
        return (0, 0); // or handle as per requirements, but problem likely expects non-empty input
    }
    
    let min_id = *all_ids.iter().min().unwrap();
    let max_id = *all_ids.iter().max().unwrap();
    
    let mut frequency = HashMap::new();
    
    for &id in &all_ids {
        *frequency.entry(id).or_insert(0) += 1;
    }
    
    let mut missing_id = 0;
    let mut duplicate_id = 0;
    
    for id in min_id..=max_id {
        match frequency.get(&id) {
            Some(&count) if count == 2 => {
                duplicate_id = id;
            }
            None => {
                missing_id = id;
            }
            _ => {}
        }
    }
    
    (missing_id, duplicate_id)
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(find_missing_and_duplicate_ids(vec![vec![5, 6, 8, 11, 9], vec![10, 12, 9]]), (7, 9));
        assert_eq!(find_missing_and_duplicate_ids(vec![vec![1, 2, 4, 7], vec![5, 3, 3]]), (6, 3));
        assert_eq!(find_missing_and_duplicate_ids(vec![vec![1, 1, 3, 4, 5]]), (2, 1)); // Edge case with minimal input
    
    }
    

}