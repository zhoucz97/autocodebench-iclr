
/*
  Calculates the area of a triangle given the coordinates of its vertices.

  Arguments:
  - x1, y1: The x and y coordinates of the first vertex.
  - x2, y2: The x and y coordinates of the second vertex.
  - x3, y3: The x and y coordinates of the third vertex.

  Returns:
  - The area of the triangle, rounded to two decimal places.

  Examples:
  - triangle_area(0.0, 0.0, 4.0, 0.0, 0.0, 3.0) returns 6.00
*/


fn triangle_area(x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32) -> f32 {
    // Calculate the area using the shoelace formula
    let area = 0.5 * ((x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)).abs();
    
    // Round to two decimal places
    (area * 100.0).round() / 100.0
}

// Example usage:


#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(triangle_area(0.0, 0.0, 4.0, 0.0, 0.0, 3.0), 6.00);
        assert_eq!(triangle_area(1.0, 2.0, 4.0, 5.0, 6.0, 3.0), 6.00); // Example test case, replace with correct area
        assert_eq!(triangle_area(0.0, 0.0, 7.0, 8.0, 9.0, 10.0), 1.00); 
    }
    

}