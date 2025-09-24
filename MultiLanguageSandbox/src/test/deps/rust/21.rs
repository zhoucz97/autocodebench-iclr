

/*
  Calculates the postage based on the weight of a package and whether express service is chosen.
  The postage is calculated as follows:
  - For weight up to and including 1000 grams, the base fee is 8 units.
  - For each additional 500 grams or part thereof, an additional fee of 4 units is added.
  - If express service (denoted by 'y') is chosen, an additional fee of 5 units is added.
  - If express service is not chosen (denoted by 'n'), no additional fee is added.

  Arguments:
  - weight: The weight of the package in grams.
  - express: A character indicating whether express service is chosen ('y' for yes, 'n' for no).

  Returns:
  - The total postage cost as an unsigned integer.

  Examples:
  - calculate_postage(1200, 'y') returns 17.
  - calculate_postage(1000, 'n') returns 8.
  - calculate_postage(1500, 'y') returns 17.
  - calculate_postage(2000, 'n') returns 16.
*/


fn calculate_postage(weight: u32, express: char) -> u32 {
    let base_fee = if weight <= 1000 { 8 } else { 8 + ((weight - 1000 + 499) / 500) * 4 };
    let express_fee = if express == 'y' { 5 } else { 0 };
    base_fee + express_fee
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(calculate_postage(1200, 'y'), 17);
        assert_eq!(calculate_postage(1000, 'n'), 8);
        assert_eq!(calculate_postage(1500, 'y'), 17);
        assert_eq!(calculate_postage(2000, 'n'), 16);
        
    }
    

}