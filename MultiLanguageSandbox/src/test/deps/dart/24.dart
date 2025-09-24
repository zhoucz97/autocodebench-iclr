
int countApplesTaoTaoCanPick(List<int> appleHeights, int maxReach)
/*Problem Description

In the courtyard of TaoTao's residence stands an apple tree that bears 10 apples each autumn. Upon the maturation of these apples, TaoTao sets out to pick the apples. TaoTao possesses a stool measuring 30 centimeters in height, which she resorts to standing on whenever the apples are beyond her direct reach.

The task is to compute the number of apples TaoTao can successfully pick, given the heights of 10 apples from the ground, alongside the maximum height TaoTao can reach with her arm fully extended. It is assumed that any apple that TaoTao touches will fall.

countApplesTaoTaoCanPick([100, 200, 150, 140, 129, 134, 167, 198, 200, 111],110) returns 5
*/
{
  int count = 0;
  for (int height in appleHeights) {
    if (height <= maxReach + 30) {
      count++;
    }
  }
  return count;
}
void testCountApplesTaoTaoCanPick() {
  // Test case 1
  List<int> appleHeights1 = [100, 200, 150, 140, 129, 134, 167, 198, 200, 111];
  int maxReach1 = 110;
  int expectedApples1 = 5;
  assert(countApplesTaoTaoCanPick(appleHeights1, maxReach1) == expectedApples1);

  // Test case 2
  List<int> appleHeights2 = [120, 130, 140, 150, 160, 170, 180, 190, 200, 210];
  int maxReach2 = 120;
  int expectedApples2 = 4;
  assert(countApplesTaoTaoCanPick(appleHeights2, maxReach2) == expectedApples2);

  // Test case 3
  List<int> appleHeights3 = [101, 102, 103, 104, 105, 106, 107, 108, 109, 110];
  int maxReach3 = 100;
  int expectedApples3 = 10;
  assert(countApplesTaoTaoCanPick(appleHeights3, maxReach3) == expectedApples3);

  // If the program reaches this point without any assertion error, print success message
  print("All test cases passed successfully.");
}

void main() {
  // Running the test function
  testCountApplesTaoTaoCanPick();
}