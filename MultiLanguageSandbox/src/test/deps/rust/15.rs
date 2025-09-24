
use std::collections::HashSet;


/*
    Calculates the number of distinct sections formed on a plane by a given set of lines.

    Each line is represented by a tuple (slope, y-intercept), following the equation y = slope * x + y-intercept.
    This function assumes lines are not overlapping and parallel lines do not contribute to new sections.

    Arguments:
    - lines: A slice of tuples, where each tuple represents a line on the plane with its slope and y-intercept as (i32, i32).

    Returns:
    - An integer representing the total number of distinct sections formed on the plane.

    Example:
    - count_plane_sections(&[(1, 1), (2, 2), (3, 3)]) returns 6
    - count_plane_sections(&[(0, 1), (0, 2)]) returns 2 (Parallel lines)
    - count_plane_sections(&[]) returns 0 (No lines)
*/

use std::collections::HashSet;

fn count_plane_sections(lines: &[(i32, i32)]) -> usize {
    let mut slopes = HashSet::new();
    for &(slope, _) in lines {
        slopes.insert(slope);
    }
    slopes.len() + 1
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(count_plane_sections(&[(1, 1), (2, 2), (3, 3)]), 6);
        assert_eq!(count_plane_sections(&[(0, 1), (0, 2)]), 3);
    }
    

}
 