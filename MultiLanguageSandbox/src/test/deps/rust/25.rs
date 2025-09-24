
/*
    Transforms a given password according to a specific algorithm. 

    The algorithm works as follows:
    - Lowercase letters are transformed to numbers based on the telephone keypad mapping (e.g., abc -> 2, def -> 3, ...).
    - Uppercase letters are first converted to lowercase and then shifted to the next letter in the alphabet (e.g., A -> b, Z -> a).
    - Numbers and other symbols remain unchanged.

    Arguments:
    - input: A string representing the password to be transformed. The password length does not exceed 100 characters.

    Returns:
    - A new string representing the transformed password.

    Examples:
    - transform_password("YUANzi1987") returns "zvbo941987"
*/

fn transform_password(input: &str) -> String {
    input.chars().map(|c| match c {
        'a'..='z' => {
            // Lowercase letters: map to telephone keypad numbers
            match c {
                'a' | 'b' | 'c' => '2',
                'd' | 'e' | 'f' => '3',
                'g' | 'h' | 'i' => '4',
                'j' | 'k' | 'l' => '5',
                'm' | 'n' | 'o' => '6',
                'p' | 'q' | 'r' | 's' => '7',
                't' | 'u' | 'v' => '8',
                'w' | 'x' | 'y' | 'z' => '9',
                _ => c, // This case shouldn't happen as we're matching 'a'..'z'
            }
        }
        'A'..='Z' => {
            // Uppercase letters: convert to lowercase and shift to next letter
            let lower = c.to_ascii_lowercase();
            if lower == 'z' {
                'a'
            } else {
                ((lower as u8) + 1) as char
            }
        }
        _ => c, // Numbers and other symbols remain unchanged
    }).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transform_password() {
        assert_eq!(transform_password("YUANzi1987"), "zvbo941987");
        assert_eq!(transform_password("ABC"), "234");
        assert_eq!(transform_password("XYZ"), "yza");
        assert_eq!(transform_password("123@#"), "123@#");
        assert_eq!(transform_password("aBcDeF"), "2c3e6");
        assert_eq!(transform_password("z"), "9");
        assert_eq!(transform_password("Z"), "a");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(transform_password("YUANzi1987"), "zvbo941987");
        assert_eq!(transform_password("ABcdE"), "bc23f");
        assert_eq!(transform_password("Zebra123!"), "a3272123!");
    }
    

}