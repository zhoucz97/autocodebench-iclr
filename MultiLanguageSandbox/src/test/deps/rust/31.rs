
/*
  Calculates the number of different ways to climb a staircase of a given number of steps, where at each step, one can either climb one step or two steps. You start at level 0.

  Arguments:
  - stairs: An integer representing the number of steps in the staircase.

  Returns:
  - An integer representing the number of different ways to climb the staircase.

  Example:
  - count_ways_to_climb(2) returns 2 (1 step + 1 step, 2 steps)
*/


fn count_ways_to_climb(stairs: i32) -> i32 {
    if stairs == 0 {
        return 1;
    }
    if stairs == 1 {
        return 1;
    }
    
    let mut a = 1; // ways(0)
    let mut b = 1; // ways(1)
    let mut c = 0;
    
    for _ in 2..=stairs {
        c = a + b;
        a = b;
        b = c;
    }
    
    b
}

#[cfg(test)]
mod tests {
    use super::*;
  
    #[test]
    fn main() {
      
        assert_eq!(count_ways_to_climb(2), 2); // 2 ways: (1+1), (2)
        assert_eq!(count_ways_to_climb(3), 3); // 3 ways: (1+1+1), (1+2), (2+1)
        assert_eq!(count_ways_to_climb(4), 5); // 5 ways: (1+1+1+1), (1+1+2), (1+2+1), (2+1+1), (2+2)
        assert_eq!(count_ways_to_climb(10), 89); 
        assert_eq!(count_ways_to_climb(24), 75025); 
        println!("All test cases passed.");
    }
    

}