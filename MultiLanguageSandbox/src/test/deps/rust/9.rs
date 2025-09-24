
/*
  Generates a sequence of integers where each subsequent integer is half of the previous one, rounded down, starting from an initial value.

  Arguments:
  - initial_value: An integer representing the starting value of the sequence.

  Returns:
  - A vector of integers representing the sequence where each element is half of its predecessor, stopping when the value becomes less than 1.

  Example:
  - generate_dividing_sequence(20) returns [20, 10, 5, 2, 1]
  - generate_dividing_sequence(5) returns [5, 2, 1]
  - generate_dividing_sequence(1) returns [1]
*/


fn generate_dividing_sequence(initial_value: i32) -> Vec<i32>{
    let mut sequence = Vec::new();
    let mut current = initial_value;
    
    while current >= 1 {
        sequence.push(current);
        current /= 2; // Integer division automatically rounds down
    }
    
    sequence
}

// Example usage:


   
#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(generate_dividing_sequence(20), vec![20, 10, 5, 2, 1]);
        assert_eq!(generate_dividing_sequence(5), vec![5, 2, 1]);
        assert_eq!(generate_dividing_sequence(1), vec![1]);
        assert_eq!(generate_dividing_sequence(15), vec![15, 7, 3, 1]);        
    }
    

}
 