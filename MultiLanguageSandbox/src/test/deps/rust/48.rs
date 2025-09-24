
/*
  Finds M positive integers whose sum equals N and whose product is maximized, returning the lexicographically smallest sequence of these integers.

  Arguments:
  - n: The sum that the M integers must equal to.
  - m: The number of integers to find.

  Returns:
  - A vector of M integers whose sum is N and whose product is maximized.

  Example:
  - max_product_partition(6, 3) returns vec![2, 2, 2]
  - max_product_partition(8, 3) returns vec![2, 3, 3]
  - max_product_partition(10, 2) returns vec![5, 5]
*/
fn max_product_partition(n: i32, m: i32) -> Vec<i32> {
    if m <= 0 || n < m {
        return vec[];
    }
    
    let base = n / m;
    let remainder = n % m;
    
    let mut result = Vec::with_capacity(m as usize);
    
    for _ in 0..(m - remainder) {
        result.push(base);
    }
    
    for _ in 0..remainder {
        result.push(base + 1);
    }
    
    result
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_product_partition(6, 3), vec![2, 2, 2]);
        assert_eq!(max_product_partition(8, 3), vec![2, 3, 3]);
        assert_eq!(max_product_partition(10, 2), vec![5, 5]);
        assert_eq!(max_product_partition(7, 3), vec![2, 2, 3]);
    }
    

}