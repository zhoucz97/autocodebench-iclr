
/*
  Calculates the total earnings of Deliv-e-droid based on the number of successfully delivered and failed deliveries.
  Deliv-e-droid earns 50 units of money for each successful delivery and loses 10 units for each failed delivery.
  Additionally, if the number of successful deliveries is greater than the number of failed ones, Deliv-e-droid receives a bonus of 500 units.

  Arguments:
  - delivered: The number of successfully delivered packages (0 <= delivered <= 100).
  - failed: The number of failed deliveries (0 <= failed <= 100).

  Returns:
  - The total earnings of Deliv-e-droid, which can be negative if the losses outweigh the gains.

  Examples:
  - calculate_droid_earnings(5, 2) returns 730.
  - calculate_droid_earnings(0, 10) returns -100.
  - calculate_droid_earnings(10, 5) returns 950.
*/

fn calculate_droid_earnings(delivered: i32, failed: i32) -> i32 {
    let base_earnings = delivered * 50 - failed * 10;
    let bonus = if delivered > failed { 500 } else { 0 };
    base_earnings + bonus
}

// Example usage:


#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(calculate_droid_earnings(5, 2), 730);
        assert_eq!(calculate_droid_earnings(0, 10), -100);
        assert_eq!(calculate_droid_earnings(10, 5), 950);
        assert_eq!(calculate_droid_earnings(3, 3), 120);
    }
    

}