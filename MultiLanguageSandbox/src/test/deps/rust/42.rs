
/*
  Finds the number with the largest prime factor in a given list of numbers.

  The function iterates through each number in the list and calculates its largest prime factor. 
  It then compares these factors to find the number with the largest prime factor.

  Arguments:
  - numbers: A slice of integers.

  Returns:
  - The number from the list that has the largest prime factor.

  Examples:
  - max_prime_factor_in_list(&[36, 38, 40, 42]) returns 38
  - max_prime_factor_in_list(&[10, 15, 21, 22]) returns 22
  - max_prime_factor_in_list(&[7, 11, 13, 19]) returns 19
*/

fn max_prime_factor_in_list(numbers: &[i32]) -> i32{
    // Helper function to find the largest prime factor of a number
    fn largest_prime_factor(mut n: i32) -> i32 {
        if n <= 1 {
            return 1;
        }
        
        let mut max_factor = 1;
        
        // Handle even numbers
        while n % 2 == 0 {
            max_factor = 2;
            n /= 2;
        }
        
        // Check odd divisors up to sqrt(n)
        let mut i = 3;
        let mut max_i = (n as f64).sqrt() as i32 + 1;
        while i <= max_i {
            while n % i == 0 {
                max_factor = i;
                n /= i;
                max_i = (n as f64).sqrt() as i32 + 1;
            }
            i += 2;
        }
        
        // If remaining n is a prime > 2
        if n > 2 {
            max_factor = n;
        }
        
        max_factor
    }
    
    // Find the number with the largest prime factor
    let mut max_num = numbers[0];
    let mut max_factor = largest_prime_factor(max_num);
    
    for &num in numbers.iter().skip(1) {
        let current_factor = largest_prime_factor(num);
        if current_factor > max_factor {
            max_factor = current_factor;
            max_num = num;
        }
    }
    
    max_num
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_max_prime_factor_in_list() {
        assert_eq!(max_prime_factor_in_list(&[36, 38, 40, 42]), 38);
        assert_eq!(max_prime_factor_in_list(&[10, 15, 21, 22]), 22);
        assert_eq!(max_prime_factor_in_list(&[7, 11, 13, 19]), 19);
        assert_eq!(max_prime_factor_in_list(&[1, 2, 3, 4, 5]), 5);
        assert_eq!(max_prime_factor_in_list(&[100, 101, 102]), 101);
        assert_eq!(max_prime_factor_in_list(&[17, 19, 23, 29]), 29);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(max_prime_factor_in_list(&[36, 38, 40, 42]), 38);
        assert_eq!(max_prime_factor_in_list(&[10, 15, 21, 22]), 22);
        assert_eq!(max_prime_factor_in_list(&[7, 11, 13, 19]), 19);
        assert_eq!(max_prime_factor_in_list(&[2, 3, 5, 7]), 7);
    }
    

}