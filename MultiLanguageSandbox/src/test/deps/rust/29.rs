
/*
  Computes the combination number C(n, m), which represents the number of ways to choose m elements from a set of n distinct elements.

  Arguments:
  - n: An unsigned 32-bit integer representing the total number of elements (n ≤ 20).
  - m: An unsigned 32-bit integer representing the number of elements to choose (m ≤ n).

  Returns:
  - The combination number C(n, m).

  Example:
  - calculate_combinations(5, 2) returns 10
  - calculate_combinations(18, 13) returns 8568
*/

fn calculate_combinations(n: u32, m: u32) -> u64 {
    // Handle edge cases
    if m == 0 || m == n {
        return 1;
    }
    
    // Use the smaller of m and n-m to minimize calculations
    let k = std::cmp::min(m, n - m);
    
    let mut result: u64 = 1;
    for i in 1..=k {
        result = result * (n - k + i) as u64 / i as u64;
    }
    
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_combinations() {
        assert_eq!(calculate_combinations(5, 2), 10);
        assert_eq!(calculate_combinations(18, 13), 8568);
        assert_eq!(calculate_combinations(0, 0), 1);
        assert_eq!(calculate_combinations(10, 0), 1);
        assert_eq!(calculate_combinations(10, 10), 1);
        assert_eq!(calculate_combinations(7, 3), 35);
        assert_eq!(calculate_combinations(20, 10), 184756);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(calculate_combinations(5, 2), 10);
        assert_eq!(calculate_combinations(18, 13), 8568);
        assert_eq!(calculate_combinations(10, 0), 1);
        assert_eq!(calculate_combinations(20, 20), 1);
        assert_eq!(calculate_combinations(15, 5), 3003);
    }
    

}