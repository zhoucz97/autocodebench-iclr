
/*
  Counts the number of student pairs whose scores in each subject (Chinese, Mathematics, English) differ by no more than 5 points, and whose total score difference is no more than 10 points. Such pairs are considered "equally matched".

  Arguments:
  - scores: A vector of tuples, each containing three integers representing the scores of a student in Chinese, Mathematics, and English, respectively.

  Returns:
  - An integer representing the number of "equally matched" student pairs.

  Example:
  - count_comparable_pairs(vec![(90, 90, 90), (85, 95, 90), (80, 100, 91)]) returns 2
*/

fn count_comparable_pairs(scores: Vec<(i32, i32, i32)>) -> i32 {
    let mut count = 0;
    let n = scores.len();
    
    for i in 0..n {
        for j in i+1..n {
            let (c1, m1, e1) = scores[i];
            let (c2, m2, e2) = scores[j];
            
            // Check individual subject differences
            let c_diff = (c1 - c2).abs();
            let m_diff = (m1 - m2).abs();
            let e_diff = (e1 - e2).abs();
            
            if c_diff <= 5 && m_diff <= 5 && e_diff <= 5 {
                // Check total score difference
                let total1 = c1 + m1 + e1;
                let total2 = c2 + m2 + e2;
                let total_diff = (total1 - total2).abs();
                
                if total_diff <= 10 {
                    count += 1;
                }
            }
        }
    }
    
    count
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_comparable_pairs(vec![(90, 90, 90), (85, 95, 90), (80, 100, 91)]), 2);
        assert_eq!(count_comparable_pairs(vec![(100, 100, 100), (95, 95, 95), (90, 90, 90)]), 0);
        assert_eq!(count_comparable_pairs(vec![(90, 85, 80), (80, 85, 90), (70, 75, 80)]), 0);
        assert_eq!(count_comparable_pairs(vec![(90, 90, 90)]), 0);
    }
    

}