
/*
  Calculates the total number of cigarettes Peter can smoke given an initial amount and a conversion rate of cigarette butts to new cigarettes.

  Arguments:
  - initial_cigarettes: The initial number of cigarettes Peter has.
  - butts_for_new: The number of cigarette butts required to exchange for one new cigarette.

  Returns:
  - The total number of cigarettes Peter can smoke including those obtained by exchanging butts for new ones.

  Example:
  - total_smoked_cigarettes(4, 3) returns 5
  - total_smoked_cigarettes(10, 3) returns 14
*/
fn total_smoked_cigarettes(initial_cigarettes: i32, butts_for_new: i32) -> i32 {
    let mut total = 0;
    let mut current_cigs = initial_cigarettes;
    let mut butts = 0;

    while current_cigs > 0 {
        // Smoke all current cigarettes
        total += current_cigs;
        butts += current_cigs;

        // Exchange butts for new cigarettes
        current_cigs = butts / butts_for_new;
        butts %= butts_for_new;
    }

    total
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_total_smoked_cigarettes() {
        assert_eq!(total_smoked_cigarettes(4, 3), 5);
        assert_eq!(total_smoked_cigarettes(10, 3), 14);
        assert_eq!(total_smoked_cigarettes(0, 3), 0);
        assert_eq!(total_smoked_cigarettes(9, 3), 13);
        assert_eq!(total_smoked_cigarettes(1, 2), 1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(total_smoked_cigarettes(4, 3), 5);
        assert_eq!(total_smoked_cigarettes(10, 3), 14);
        assert_eq!(total_smoked_cigarettes(1, 2), 1); // Edge case: Not enough to exchange
        assert_eq!(total_smoked_cigarettes(100, 5), 124); // Larger numbers
    }
    

}