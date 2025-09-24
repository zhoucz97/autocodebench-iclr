
/*
  Counts the number of uppercase letters, lowercase letters, and digits in a given string.

  Arguments:
  - input: A string that may contain uppercase and lowercase English letters, as well as digits.

  Returns:
  - A tuple containing three usize values representing the counts of uppercase letters, lowercase letters, and digits, respectively.

  Example:
  - count_character_types("Rust3IsFun!") returns (3, 6, 1)
  - count_character_types("1234") returns (0, 0, 4)
  - count_character_types("HelloWorld!") returns (2, 8, 0)
*/

fn count_character_types(input: &str) -> (usize, usize, usize){
    let mut uppercase = 0;
    let mut lowercase = 0;
    let mut digits = 0;

    for c in input.chars() {
        if c.is_uppercase() {
            uppercase += 1;
        } else if c.is_lowercase() {
            lowercase += 1;
        } else if c.is_ascii_digit() {
            digits += 1;
        }
    }

    (uppercase, lowercase, digits)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_character_types() {
        assert_eq!(count_character_types("Rust3IsFun"), (3, 6, 1));
        assert_eq!(count_character_types("1234"), (0, 0, 4));
        assert_eq!(count_character_types("HelloWorld"), (2, 8, 0));
        assert_eq!(count_character_types(""), (0, 0, 0));
        assert_eq!(count_character_types("A1b2C3d4"), (2, 2, 4));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_character_types("Rust3IsFun!"), (3, 6, 1));
        assert_eq!(count_character_types("1234"), (0, 0, 4));
        assert_eq!(count_character_types("HelloWorld!"), (2, 8, 0));
        assert_eq!(count_character_types("NoDigitsHere"), (3, 9, 0));        
    }
    

}