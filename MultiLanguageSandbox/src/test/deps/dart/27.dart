
int minRemainingSpace(int V, List<int> volumes)
/*There is a box with a capacity of $V$, and there are $n$ items, each with a volume.

Now, from these $n$ items, any number of items can be selected for packing into the box (including the option of not selecting any), with the objective of minimizing the remaining space in the box. Output this minimum value.

Input Format

The first line contains a single integer $V$, indicating the capacity of the box.

The second line contains a single integer $n$, indicating the total number of items.

The following $n$ lines each contain a positive integer, indicating the volume of the $i^{th}$ item.

Output Format

- A single line containing one integer, indicating the minimum remaining space in the box.

example
minRemainingSpace(30,[10,14,20]) => 0
*/
{
  bool dp[V + 1];
  for (int i = 0; i <= V; i++) {
    dp[i] = false;
  }
  dp[0] = true;

  for (int volume in volumes) {
    for (int i = V; i >= volume; i--) {
      if (dp[i - volume]) {
        dp[i] = true;
      }
    }
  }

  int maxVolume = 0;
  for (int i = V; i >= 0; i--) {
    if (dp[i]) {
      maxVolume = i;
      break;
    }
  }

  return V - maxVolume;
}
void testMinRemainingSpace() {
  assert(minRemainingSpace(24, [8, 3, 12, 7, 9, 7]) == 0);
  assert(minRemainingSpace(10, [5, 3, 2, 4]) == 0);
  assert(minRemainingSpace(30, [10, 14, 20]) == 0);

  print("All test cases passed successfully.");
}

void main() {
  testMinRemainingSpace();
}