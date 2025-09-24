
/*
  Calculates the number of characters in a given title string, excluding spaces and newline characters.

  Arguments:
  - title: A string that may contain uppercase and lowercase English letters, digits, spaces, and newline characters.

  Returns:
  - The count of characters in the title, excluding spaces and newline characters.

  Examples:
  - count_title_chars("234") returns 3
  - count_title_chars("Ca 45") returns 4
  - count_title_chars("Hello\nWorld") returns 10
*/


fn count_title_chars(title: &str) -> usize {
    title.chars()
        .filter(|c| *c != ' ' && *c != '\n')
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_title_chars("234"), 3);
        assert_eq!(count_title_chars("Ca 45"), 4);
        assert_eq!(count_title_chars("Hello\nWorld"), 10);
    }
    

}