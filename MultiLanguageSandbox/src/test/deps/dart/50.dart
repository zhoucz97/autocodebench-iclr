
int countExcellentSplits(String s)
/*
Given a string, determine the number of substrings with excellent decomposition. A decomposition is considered excellent if it can be written in the form "AABB", where "A" and "B" are non-empty substrings.

countExcellentSplits("abcdabc")
0
countExcellentSplits("aaabaaa")
2
*/
{
  int count = 0;
  int n = s.length;
  
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      String substring = s.substring(i, j + 1);
      int len = substring.length;
      if (len % 2 != 0) continue; // must be even to split into two halves
      
      int half = len ~/ 2;
      String left = substring.substring(0, half);
      String right = substring.substring(half);
      
      // Check if left is AA (i.e., first half of left == second half of left)
      bool isLeftAA = false;
      if (left.length % 2 == 0) {
        int leftHalfLen = left.length ~/ 2;
        String leftFirst = left.substring(0, leftHalfLen);
        String leftSecond = left.substring(leftHalfLen);
        if (leftFirst == leftSecond) {
          isLeftAA = true;
        }
      }
      
      // Check if right is BB (i.e., first half of right == second half of right)
      bool isRightBB = false;
      if (right.length % 2 == 0) {
        int rightHalfLen = right.length ~/ 2;
        String rightFirst = right.substring(0, rightHalfLen);
        String rightSecond = right.substring(rightHalfLen);
        if (rightFirst == rightSecond) {
          isRightBB = true;
        }
      }
      
      if (isLeftAA && isRightBB) {
        count++;
      }
    }
  }
  
  return count;
}
void main() {
  check(countExcellentSplits);
}

void check(int Function(String) countExcellentSplits) {
  assert(countExcellentSplits("abcdabc") == 0);
  assert(countExcellentSplits("aabbbb") == 3);
  assert(countExcellentSplits("cccccc") == 4);
}