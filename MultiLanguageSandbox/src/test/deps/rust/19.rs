
use std::f64::consts::PI;

/*
  Calculates the minimum number of buckets a giraffe needs to drink to quench its thirst, given the size of the buckets.
  
  A giraffe needs to drink 20 liters of water to quench its thirst. This function determines how many buckets of water it would need, based on the volume of each bucket. The buckets are cylindrical in shape, with given height and radius.

  Arguments:
  - height: The height of the bucket in centimeters (integer).
  - radius: The radius of the bucket's base in centimeters (integer).

  Returns:
  - An integer representing the minimum number of buckets needed for the giraffe to quench its thirst.

  Example:
  - min_buckets_needed(23, 11) returns 3
  - min_buckets_needed(30, 10) returns 3
*/

fn min_buckets_needed(height: i32, radius: i32) -> i32 {
    // Calculate the volume of one bucket in cm^3
    let volume_cm3: f64 = PI * (radius as f64).powi(2) * (height as f64);
    // Convert volume to liters (1 liter = 1000 cm^3)
    let volume_liters = volume_cm3 / 1000.0;
    // Calculate the minimum number of buckets needed (ceiling of 20 / volume_liters)
    let buckets = (20.0 / volume_liters).ceil() as i32;
    buckets
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(min_buckets_needed(23, 11), 3);
        assert_eq!(min_buckets_needed(30, 10), 3);
        assert_eq!(min_buckets_needed(50, 5), 6);
        assert_eq!(min_buckets_needed(40, 8), 3);    
    }
    

}