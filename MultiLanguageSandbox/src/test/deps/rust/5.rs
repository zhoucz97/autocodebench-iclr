

 /*
  For a given list of input numbers, calculate Mean Absolute Deviation
  around the mean of this dataset.
  Mean Absolute Deviation is the average absolute difference between each
  element and a centerpoint (mean in this case):
  MAD = average | x - x_mean |
  
 */
 
 use std::{slice::Iter, cmp::{max, self}, mem::replace, collections::{HashSet, HashMap}, ops::Index, ascii::AsciiExt};
 use rand::Rng;
 use regex::Regex;
 use md5;
 use std::any::{Any, TypeId};
 
 fn mean_absolute_deviation(numbers:Vec<f32>) -> f32{
 
    if numbers.is_empty() {
        return 0.0; // or you could return an error, but the problem doesn't specify
    }

    // Calculate the mean of the numbers
    let sum: f32 = numbers.iter().sum();
    let mean = sum / numbers.len() as f32;

    // Calculate the absolute deviations from the mean
    let absolute_deviations: Vec<f32> = numbers.iter()
        .map(|&x| (x - mean).abs())
        .collect();

    // Calculate the mean of the absolute deviations
    let mad_sum: f32 = absolute_deviations.iter().sum();
    mad_sum / numbers.len() as f32
}


 #[cfg(test)]
 mod tests {
  use super::*;
 
  #[test]
  fn test_mean_absolute_deviation() {
  assert!(mean_absolute_deviation(vec![1.0, 2.0, 3.0]) - 2.0 / 3.0 < 0.000001);
  assert!(mean_absolute_deviation(vec![1.0, 2.0, 3.0, 4.0]) - 1.0 < 0.000001);
  assert!(mean_absolute_deviation(vec![1.0, 2.0, 3.0, 4.0, 5.0]) - 6.0 / 5.0 < 0.000001);
  }
 
 }
 