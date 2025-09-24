
/*
  Calculates the minimum total distance from all residents to a post office located within their city. The city is laid out in a grid, with streets running north-south and east-west, forming blocks. 

  The distance between two points (x1, y1) and (x2, y2) is defined as |x1 - x2| + |y1 - y2|.

  Arguments:
  - residents: A slice of tuples, each representing the (x, y) coordinates of a resident's location.

  Returns:
  - The minimum total distance from all residents to the post office.

  Examples:
  - minimum_distance_to_post_office(&[(1, 2), (2, 2), (1, 3), (3, -2), (3, 3)]) returns 10
  - minimum_distance_to_post_office(&[(0, 0), (1, 1)]) returns 2
*/

fn minimum_distance_to_post_office(residents: &[(i32, i32)]) -> i32 {
    if residents.is_empty() {
        return 0;
    }
    
    // Extract x and y coordinates into separate vectors
    let mut x_coords: Vec<i32> = residents.iter().map(|&(x, _)| x).collect();
    let mut y_coords: Vec<i32> = residents.iter().map(|&(_, y)| y).collect();
    
    // Sort the coordinates to find medians
    x_coords.sort();
    y_coords.sort();
    
    let n = residents.len();
    let mid = n / 2;
    
    // The median for x and y coordinates
    let x_median = x_coords[mid];
    let y_median = y_coords[mid];
    
    // Calculate the total distance
    residents.iter().fold(0, |acc, &(x, y)| {
        acc + (x - x_median).abs() + (y - y_median).abs()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(minimum_distance_to_post_office(&[(1, 2), (2, 2), (1, 3), (3, -2), (3, 3)]), 10);
        assert_eq!(minimum_distance_to_post_office(&[(0, 0), (1, 1)]), 2);
    
    }

}