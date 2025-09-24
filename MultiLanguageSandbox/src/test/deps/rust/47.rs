
/*
  Compares the area of a square with side length 'a' to the area of a rectangle with dimensions 'b' x 'c'.
  Returns a string indicating whether the square ('Alice') or the rectangle ('Bob') has a larger area.
  
  Arguments:
  - a: An usize representing the side length of the square.
  - b: An usize representing the length of the rectangle.
  - c: An usize representing the width of the rectangle.

  Returns:
  - A string that is either 'Alice' if the square's area is larger, or 'Bob' if the rectangle's area is larger.

  Example:
  - compare_area(5, 4, 6) returns "Alice"
  - compare_area(7, 5, 10) returns "Bob"
*/
fn compare_area(a: usize, b: usize, c: usize) -> String {
    let square_area = a * a;
    let rectangle_area = b * c;

    if square_area > rectangle_area {
        "Alice".to_string()
    } else {
        "Bob".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(compare_area(5, 4, 6), "Alice");
        assert_eq!(compare_area(7, 5, 10), "Bob");
        assert_eq!(compare_area(6, 3, 8), "Alice");
        assert_eq!(compare_area(9, 4, 5), "Alice");
    }
    

}