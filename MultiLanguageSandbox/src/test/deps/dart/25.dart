
int countRemainingTrees(int roadLength, List<List<int>> regions)
/*Problem Description
Adjacent to a school's main gate, there is a road of length $l$ meters, lined with a sequence of trees. The spacing between each pair of adjacent trees is 1 meter. The road can be conceptualized as a numerical axis, with one end at the position 0 on the axis and the other end at position $l$; a tree is planted at every integer point on this axis, i.e., at $0,1,2,\dots,l$.
Some sections of this road are designated for subway construction. These sections are defined by their starting and ending points on the numerical axis. It is known that the coordinates of any section's starting and ending points are integers, and there may be overlaps between sections. The current task involves removing the trees within these sections, including the trees located at the endpoints of each section. Your objective is to calculate the number of trees remaining on the road after these trees have been removed.

Input Format
The first line contains two integers, representing respectively the length of the road $l$ and the number of sections $m$.
The following $m$ lines each contain two integers $u, v$, representing the coordinates of a section's starting and ending points.

Output Format
Output a single integer on one line, representing the number of trees remaining on the road after the specified trees have been removed.

Sample 
countRemainingTrees(500,[[150, 300], [301, 450]]) => 200
*/
{
  Set<int> removedPositions = Set();
  
  for (var region in regions) {
    int u = region[0];
    int v = region[1];
    // Ensure u <= v to handle cases where u might be greater than v
    if (u > v) {
      int temp = u;
      u = v;
      v = temp;
    }
    // Add all positions from u to v inclusive to the set
    for (int i = u; i <= v; i++) {
      removedPositions.add(i);
    }
  }
  
  int totalTrees = roadLength + 1;
  int removedCount = removedPositions.length;
  return totalTrees - removedCount;
}
void testCountRemainingTrees() {
  // Test case 1: Non-overlapping regions
  int roadLength1 = 500;
  List<List<int>> regions1 = [
    [150, 300],
    [301, 450]
  ];
  int expectedRemaining1 = 200; // Trees from 0-149, 451-500
  assert(countRemainingTrees(roadLength1, regions1) == expectedRemaining1);

  // Test case 2: Partially overlapping regions
  int roadLength2 = 500;
  List<List<int>> regions2 = [
    [100, 200],
    [150, 300],
    [470, 471]
  ];
  int expectedRemaining2 = 298; // Trees from 0-99, 301-469, 472-500
  assert(countRemainingTrees(roadLength2, regions2) == expectedRemaining2);

  // Test case 3: All trees are removed
  int roadLength3 = 10;
  List<List<int>> regions3 = [
    [0, 10]
  ];
  int expectedRemaining3 = 0; // All trees are removed
  assert(countRemainingTrees(roadLength3, regions3) == expectedRemaining3);

  print("All test cases passed successfully.");
}

void main() {
  testCountRemainingTrees();
}