
/*
  Determines if Yuanzi can win a horse race against an opponent by optimally arranging his horses.

  The function compares the speeds of Yuanzi's horses to those of the opponent's horses. Each horse has a fixed speed and there are no ties. Yuanzi wins if he wins more than half of the individual races.

  Arguments:
  - yuanzi_horses: An array of integers representing the speeds of Yuanzi's horses.
  - opponent_horses: An array of integers representing the speeds of the opponent's horses.

  Returns:
  - A boolean value: `true` if Yuanzi can win the race, `false` otherwise.

  Examples:
  - can_win_horse_race(&[2, 3, 3, 4, 5], &[1, 2, 3, 4, 5]) returns true
  - can_win_horse_race(&[2, 2, 1, 2], &[2, 2, 3, 1]) returns false
*/

fn can_win_horse_race(yuanzi_horses: &[i32], opponent_horses: &[i32]) -> bool {
    let mut yuanzi_sorted = yuanzi_horses.to_vec();
    let mut opponent_sorted = opponent_horses.to_vec();
    
    yuanzi_sorted.sort_unstable();
    opponent_sorted.sort_unstable();
    
    let mut wins = 0;
    let mut i = 0; // pointer for yuanzi's horses
    let mut j = 0; // pointer for opponent's horses
    
    while i < yuanzi_sorted.len() && j < opponent_sorted.len() {
        if yuanzi_sorted[i] > opponent_sorted[j] {
            wins += 1;
            i += 1;
            j += 1;
        } else {
            i += 1;
        }
    }
    
    wins > (yuanzi_horses.len() as i32) / 2
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(can_win_horse_race(&[2, 3, 3, 4, 5], &[1, 2, 3, 4, 5]), true);
        assert_eq!(can_win_horse_race(&[2, 2, 1, 2], &[2, 2, 3, 1]), false);
    }
    

}