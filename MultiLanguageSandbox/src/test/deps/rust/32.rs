

/*
  Finds all ranges of consecutive natural numbers whose sum equals the given target sum.

  The function identifies all possible contiguous sequences of natural numbers where the sum of all numbers in the sequence equals the target sum. The function returns a vector of tuples, each tuple representing the start and end of a sequence.

  Arguments:
  - target_sum: An integer representing the target sum (10 <= target_sum <= 2,000,000).

  Returns:
  - A vector of tuples. Each tuple contains two integers: the first and last number in a contiguous sequence that sums up to the target sum.

  Example:
  - find_sum_sequence_ranges(10000) returns vec![(18, 142), (297, 328), (388, 412), (1998, 2002)]

*/
fn find_sum_sequence_ranges(target_sum: i32) -> Vec<(i32, i32)> {
    let mut result = Vec::new();
    let double_target = 2 * target_sum;
    
    // The maximum possible n is such that n(n+1)/2 <= target_sum, but for practical purposes, we can limit n to sqrt(2*target_sum) or similar.
    // However, to cover all cases, we can iterate n from 2 up to a reasonable limit where n^2 is less than 2*target_sum.
    let max_n = ((2 * target_sum) as f64).sqrt() as i32 + 2;
    
    for n in 2..=max_n {
        if double_target % n != 0 {
            continue;
        }
        let s = double_target / n;
        // Now, solve for a and b: a + b = s, b = a + n - 1
        // So 2a + n - 1 = s => a = (s - n + 1) / 2
        let a = (s - n + 1) / 2;
        let b = a + n - 1;
        if a > 0 && b > 0 && (a + b) == s {
            result.push((a, b));
        }
    }
    
    // The sequences might be found in reverse order of their lengths, so we sort them by start value.
    result.sort_by_key(|&(a, _)| a);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(find_sum_sequence_ranges(10000), vec![(18, 142), (297, 328), (388, 412), (1998, 2002)]);
        assert_eq!(find_sum_sequence_ranges(15), vec![(1, 5), (4, 6), (7, 8)]);
        assert_eq!(find_sum_sequence_ranges(28), vec![(1, 7)]);
        assert_eq!(find_sum_sequence_ranges(21334), vec![(5332, 5335)]);
        println!("All test cases passed!");
    }
    

}