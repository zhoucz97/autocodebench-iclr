

/*
  Calculates the difference between the kth largest and kth smallest numbers in a given list of integers.
  If the kth largest or smallest number does not exist, the function returns None.

  Arguments:
  - numbers: A Vec<i32> representing the sequence of non-negative integers.
  - k: A usize value representing the kth position.

  Returns:
  - An Option<i32> value representing the difference if both kth largest and smallest numbers exist, otherwise None.

  Example:
  - kth_largest_minus_kth_smallest(vec![3, 1, 4, 1, 5, 9, 2], 2) returns Some(4) // (5 - 1)
  - kth_largest_minus_kth_smallest(vec![10, 20, 30], 4) returns None
*/
fn kth_largest_minus_kth_smallest(numbers: Vec<i32>, k: usize) -> Option<i32> {
    let mut sorted = numbers.clone();
    sorted.sort_unstable();
    
    let len = sorted.len();
    if k > len {
        return None;
    }
    
    let kth_smallest = sorted[k - 1];
    let kth_largest = sorted[len - k];
    
    Some(kth_largest - kth_smallest)
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
     
        assert_eq!(kth_largest_minus_kth_smallest(vec![3, 1, 4, 1, 5, 9, 2], 2), Some(4)); // 5 - 1 = 4
        assert_eq!(kth_largest_minus_kth_smallest(vec![10, 20, 30], 4), None);
        assert_eq!(kth_largest_minus_kth_smallest(vec![7, 7, 7, 7], 1), Some(0)); // 7 - 7 = 0
        
        
    }
    

}