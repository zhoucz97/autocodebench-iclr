
/*
  Calculates the maximum total value of herbs that can be collected within a given time limit from a set of herbs, each with its own time requirement and value.

  Arguments:
  - total_time: A u32 representing the total time available for collecting herbs.
  - herbs: A slice of tuples, where each tuple contains two u32 values representing the time required to collect a herb and its value, respectively.

  Returns:
  - A u32 representing the maximum total value of herbs that can be collected within the given time.

  Examples:
  - max_herbs_value(70, &[(71, 100), (69, 1), (1, 2)]) returns 3
  - max_herbs_value(10, &[(5, 60), (4, 40), (3, 50)]) returns 100
  - max_herbs_value(5, &[(2, 20), (3, 30)]) returns 30
*/

fn max_herbs_value(total_time: u32, herbs: &[(u32, u32)]) -> u32 {
    let mut dp = vec![0; (total_time + 1) as usize];
    
    for &(time, value) in herbs {
        for t in (time..=total_time).rev() {
            if dp[(t - time) as usize] + value > dp[t as usize] {
                dp[t as usize] = dp[(t - time) as usize] + value;
            }
        }
    }
    
    dp[total_time as usize]
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_herbs_value(70, &[(71, 100), (69, 1), (1, 2)]), 3);
        assert_eq!(max_herbs_value(10, &[(5, 60), (4, 40), (3, 50)]), 110);
        assert_eq!(max_herbs_value(5, &[(2, 20), (3, 30)]), 50);
    }
    

}