
use std::collections::HashSet;


/*
  Calculates the number of distinct weights that can be measured using a given set of weights on a balance scale.
  The weights can be placed on either side of the scale, and the same weight can't be used more than once.

  Arguments:
  - weights: A slice of integers representing the weights.

  Returns:
  - The total number of distinct weights that can be measured.

  Example:
  - distinct_weights_count(&[1, 4, 6]) returns 10
  - distinct_weights_count(&[2, 3]) returns 5 (weights: 1, 2, 3, 5)
  - distinct_weights_count(&[]) returns 1 (only zero can be measured)
*/


fn distinct_weights_count(weights: &[i32]) -> i32 {
    let mut sums = HashSet::new();
    sums.insert(0); // Start with zero, which is always possible
    
    for &weight in weights {
        let current_sums: Vec<i32> = sums.iter().cloned().collect();
        for &sum in &current_sums {
            sums.insert(sum + weight);
            sums.insert(sum - weight);
        }
    }
    
    sums.len() as i32
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(distinct_weights_count(&[1, 4, 6]), 10);
        assert_eq!(distinct_weights_count(&[2, 3]), 4);
        assert_eq!(distinct_weights_count(&[]), 0);
        assert_eq!(distinct_weights_count(&[1, 2, 2]), 5);  
        
    }
    

}