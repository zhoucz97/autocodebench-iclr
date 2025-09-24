
int findTopmostCarpetIndex(List<List<int>> carpets, List<int> point)
/*Finds the index of the topmost carpet that covers a given point.

Takes a list of carpets, where each carpet is represented by a list of four integers
[a, b, g, k], which define a carpet's bottom-left corner (a, b) and its dimensions g (width)
and k (height). The function also takes a point represented by a list of two integers [x, y],
which define the coordinates of the point.

The carpets are layered in the order they appear in the list, with the first carpet at the bottom.
The function iterates through the carpets from the topmost to the bottommost. It returns the
1-based index of the first carpet that covers the point. If no carpet covers the point, the function
returns -1.

Args:
  carpets (List<List<int>>): The list of carpets information.
  point (List<int>): The coordinates of the point to check.

Returns:
  int: The 1-based index of the topmost carpet covering the point, or -1 if the point is not covered.

Example:
  findTopmostCarpetIndex([[0, 0, 4, 4], [1, 1, 3, 3]], [2, 2]); // Returns 2

  findTopmostCarpetIndex([[0, 0, 4, 4], [5, 5, 2, 2]], [6, 6]); // Returns -1
*/
{
  // Iterate through the carpets from topmost to bottommost (reverse order)
  for (int i = carpets.length - 1; i >= 0; i--) {
    List<int> carpet = carpets[i];
    int a = carpet[0]; // x-coordinate of bottom-left corner
    int b = carpet[1]; // y-coordinate of bottom-left corner
    int g = carpet[2]; // width
    int k = carpet[3]; // height
    
    int x = point[0];
    int y = point[1];
    
    // Check if the point is within the carpet's boundaries
    if (x >= a && x <= a + g && y >= b && y <= b + k) {
      // Return 1-based index (i + 1)
      return i + 1;
    }
  }
  
  // If no carpet covers the point, return -1
  return -1;
}
void testFindTopmostCarpetIndex() {
  // TestCase 1: Point lies on the second carpet which is on top of the first one
  List<List<int>> carpets1 = [
    [0, 0, 4, 4], // Carpet 1 (bottom)
    [1, 1, 3, 3], // Carpet 2 (top)
  ];
  List<int> point1 = [2, 2];
  assert(findTopmostCarpetIndex(carpets1, point1) == 2, 'Test case 1 failed');

  // TestCase 2: Point lies outside any carpet
  List<List<int>> carpets2 = [
    [0, 0, 4, 4],
    [5, 5, 2, 2],
  ];
  List<int> point2 = [6, 6];
  assert(findTopmostCarpetIndex(carpets2, point2) == 2, 'Test case 2 failed');

  // TestCase 3: Point lies on the only carpet
  List<List<int>> carpets3 = [
    [0, 0, 5, 5], // Only one carpet
  ];
  List<int> point3 = [3, 3];
  assert(findTopmostCarpetIndex(carpets3, point3) == 1, 'Test case 3 failed');

  print('All test cases passed!');
}

void main() {
  // Call the test function to run the test cases
  testFindTopmostCarpetIndex();
}