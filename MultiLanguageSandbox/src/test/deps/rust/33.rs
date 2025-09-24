
/*
  Converts a binary string (consisting of '0's and '1's) into its inverted form. 
  Every '0' in the input string is replaced with a '1', and every '1' is replaced with a '0'.

  Arguments:
  - input: A string consisting of binary digits ('0' and '1').

  Returns:
  - A new string representing the inverted binary sequence of the input string.

  Example:
  - invert_binary_string("0110") returns "1001"
  - invert_binary_string("1111") returns "0000"
  - invert_binary_string("0000") returns "1111"
*/
fn invert_binary_string(input: &str) -> String {
    input.chars()
        .map(|c| match c {
            '0' => '1',
            '1' => '0',
            _ => panic!("Invalid binary character: {}", c),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
   
        assert_eq!(invert_binary_string("0110100100100"), "1001011011011");
        assert_eq!(invert_binary_string("1000000010000000000"), "0111111101111111111");
        assert_eq!(invert_binary_string("11110000"), "00001111");
        assert_eq!(invert_binary_string("0000"), "1111");
        println!("All test cases passed!");
        
        
    }
    

}