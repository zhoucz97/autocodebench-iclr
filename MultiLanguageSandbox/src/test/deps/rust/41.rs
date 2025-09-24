

/*
  Calculates the maximum possible sum of absolute differences between pairs of elements from two arrays.
  
  Arguments:
  - a: An array of integers.
  - b: An array of integers, of the same length as 'a'.
  
  Returns:
  - The maximum sum of absolute differences that can be obtained by pairing each element from 'a' with one from 'b'.

  Examples:
  - max_pairing_difference(&[2, 5, 6, 3], &[1, 4, 6, 7]) returns 14.
  - max_pairing_difference(&[1, 2], &[3, 4]) returns 4.
  - max_pairing_difference(&[10, 20, 30], &[30, 20, 10]) returns 60.
*/

fn max_pairing_difference(a: &[i32], b: &[i32]) -> i64 {
    let mut a_sorted = a.to_vec();
    let mut b_sorted = b.to_vec();
    
    a_sorted.sort();
    b_sorted.sort_by(|x, y| y.cmp(x)); // Sort in descending order
    
    let mut sum = 0i64;
    for (ai, bi) in a_sorted.iter().zip(b_sorted.iter()) {
        sum += (ai - bi).abs() as i64;
    }
    
    sum
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_pairing_difference(&[2, 5, 6, 3], &[1, 4, 6, 7]), 14);
        assert_eq!(max_pairing_difference(&[1, 2], &[3, 4]), 4);
        assert_eq!(max_pairing_difference(&[10, 20, 30], &[30, 20, 10]), 40);
    }
    
}