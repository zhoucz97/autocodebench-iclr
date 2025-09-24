
/*
  Finds the highest-valued dream that is within the user's ability.

  This function takes an array of dream values and the user's ability score. It returns the highest dream value that does not exceed the user's ability.

  Arguments:
  - dreams: An array of unsigned 32-bit integers, each representing the ability required for a dream.
  - ability: An unsigned 32-bit integer representing the user's ability score.

  Returns:
  - The value of the dream that is the highest but still within the user's ability.

  Examples:
  - max_dream_within_ability(&[50, 69, 240, 135, 98], 100) returns 98
  - max_dream_within_ability(&[20, 40, 60], 50) returns 40
  - max_dream_within_ability(&[10, 20, 30], 5) returns 0 (No dream within ability)
*/


fn max_dream_within_ability(dreams: &[u32], ability: u32) -> u32 {
    let mut max_dream = 0;
    
    for &dream in dreams {
        if dream <= ability && dream > max_dream {
            max_dream = dream;
        }
    }
    
    max_dream
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_max_dream_within_ability() {
        assert_eq!(max_dream_within_ability(&[50, 69, 240, 135, 98], 100), 98);
        assert_eq!(max_dream_within_ability(&[20, 40, 60], 50), 40);
        assert_eq!(max_dream_within_ability(&[10, 20, 30], 5), 0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_dream_within_ability(&[50, 69, 240, 135, 98], 100), 98);
        assert_eq!(max_dream_within_ability(&[20, 40, 60], 50), 40);
        assert_eq!(max_dream_within_ability(&[10, 20, 30], 5), 0); // No dream within ability
        assert_eq!(max_dream_within_ability(&[120, 200, 300, 400], 350), 300);
    
    }
    

}