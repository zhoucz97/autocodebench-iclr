
/*
  Validates the correctness of a given ISBN number.

  An ISBN number is a string formatted as "x-xxx-xxxxx-x", where 'x' represents a digit, and '-' is a separator. The last digit is a check digit, calculated as the sum of the first 9 digits each multiplied by their position (1 to 9), modulo 11. If the result is 10, the check digit is 'X'.

  Arguments:
  - isbn: A string representing an ISBN number.

  Returns:
  - A string response. If the ISBN is correct, returns "Right". Otherwise, returns the corrected ISBN number.

  Example:
  - validate_isbn("0-670-82162-4") returns "Right"
  - validate_isbn("0-670-82162-0") returns "0-670-82162-4"
*/


fn validate_isbn(isbn: &str) -> String {
    // Check the basic format: "x-xxx-xxxxx-x" where x is digit
    if !isbn.chars().enumerate().all(|(i, c)| {
        match i {
            0 | 2..=4 | 6..=10 => c.is_ascii_digit(),
            1 | 5 => c == '-',
            _ => false,
        }
    }) || isbn.len() != 13 {
        return isbn.to_string(); // Return original if format is wrong
    }

    let digits: Vec<u32> = isbn.chars()
        .filter(|c| c.is_ascii_digit())
        .map(|c| c.to_digit(10).unwrap())
        .collect();

    if digits.len() != 10 {
        return isbn.to_string();
    }

    // Calculate the check digit
    let sum: u32 = digits.iter()
        .take(9)
        .enumerate()
        .map(|(i, &d)| d * (i as u32 + 1))
        .sum();

    let check_digit = match sum % 11 {
        10 => 'X',
        n => char::from_digit(n, 10).unwrap(),
    };

    if digits[9] == check_digit.to_digit(10).unwrap_or(10) {
        "Right".to_string()
    } else {
        // Construct the corrected ISBN
        let mut corrected = isbn.chars().collect::<Vec<char>>();
        corrected[12] = check_digit;
        corrected.into_iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_isbn() {
        assert_eq!(validate_isbn("0-670-82162-4"), "Right");
        assert_eq!(validate_isbn("0-670-82162-0"), "0-670-82162-4");
        assert_eq!(validate_isbn("0-670-82162-X"), "Right");
        assert_eq!(validate_isbn("0-670-82162-5"), "0-670-82162-X");
        assert_eq!(validate_isbn("invalid-isbn"), "invalid-isbn");
        assert_eq!(validate_isbn("0-670-82162"), "0-670-82162");
        assert_eq!(validate_isbn("0-670-82162-12"), "0-670-82162-12");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(validate_isbn("0-670-82162-4"), "Right");
        assert_eq!(validate_isbn("0-670-82162-0"), "0-670-82162-4");
        assert_eq!(validate_isbn("0-123-45678-9"), "Right");
        assert_eq!(validate_isbn("1-234-56789-X"), "Right");
    }
    

}