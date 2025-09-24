
/*
  Expands a compressed string where consecutive characters are represented by the character followed by its count.
  The count is a single digit (1-9), and it specifies how many times the character appears consecutively. If a character appears only once, it's not followed by a count.

  Arguments:
  - compressed: A string containing the compressed format, consisting of characters and single-digit counts.

  Returns:
  - A string representing the expanded form of the compressed string.

  Example:
  - expand_compressed_string("a3b2c") returns "aaabbc"
  - expand_compressed_string("H3el5o2") returns "HHHellllloo"
  - expand_compressed_string("x9y1z2") returns "xxxxxxxxxyzz"
*/

fn expand_compressed_string(compressed: &str) -> String {
    let mut result = String::new();
    let mut chars = compressed.chars().peekable();

    while let Some(c) = chars.next() {
        if c.is_ascii_digit() {
            // This shouldn't happen as per the problem statement (digits follow characters)
            continue;
        }

        let mut count = 1;
        if let Some(&next_char) = chars.peek() {
            if next_char.is_ascii_digit() {
                count = next_char.to_digit(10).unwrap();
                chars.next(); // consume the digit
            }
        }

        for _ in 0..count {
            result.push(c);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_compressed_string() {
        assert_eq!(expand_compressed_string("a3b2c"), "aaabbc");
        assert_eq!(expand_compressed_string("H3el5o2"), "HHHellllloo");
        assert_eq!(expand_compressed_string("x9y1z2"), "xxxxxxxxxyzz");
        assert_eq!(expand_compressed_string("abc"), "abc");
        assert_eq!(expand_compressed_string("a1b1c1"), "abc");
        assert_eq!(expand_compressed_string("z9"), "zzzzzzzzz");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(expand_compressed_string("a3b2c"), "aaabbc");
        assert_eq!(expand_compressed_string("H3el5o2"), "HHHellllloo");
        assert_eq!(expand_compressed_string("x9y1z2"), "xxxxxxxxxyzz");
        assert_eq!(expand_compressed_string("r4s"), "rrrrs");
        
    }
    

}
 