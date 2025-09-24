
int minimumSwaps(List<int> train)
/*
Adjacent to an old-fashioned train station lies a bridge with a deck capable of horizontally rotating around the central pier of the river. A worker at the station discovered that the bridge could accommodate a maximum of two carriages. By rotating the bridge 180 degrees, the positions of two adjacent carriages could be interchanged, thus allowing for the reordering of the carriages' sequence. Consequently, he took on the responsibility of using the bridge to organize the incoming carriages in ascending order based on their carriage numbers. Following his retirement, the train station decided to automate this task. A crucial part of this automation involves developing a program that takes the initial order of the carriages and calculates the minimum number of steps required to sort them.

## Input Format

train: A list of integers representing the initial order of the carriages.

## Output Format

A single integer indicating the minimum number of rotations required.

## Sample Case #1
minimumSwaps([4, 3, 2, 1]) => 6

*/
{
  int swaps = 0;
  bool swapped;
  int n = train.length;

  do {
    swapped = false;
    for (int i = 0; i < n - 1; i++) {
      if (train[i] > train[i + 1]) {
        // Swap the elements
        int temp = train[i];
        train[i] = train[i + 1];
        train[i + 1] = temp;
        swaps++;
        swapped = true;
      }
    }
    // After each pass, the largest unsorted element is in its correct position
    n--;
  } while (swapped);

  return swaps;
}
void testMinimumSwaps() {
  assert(minimumSwaps([4, 3, 2, 1]) == 2);
  assert(minimumSwaps([1, 3, 5, 2, 4, 6]) == 3);
  assert(minimumSwaps([1, 5, 4, 3, 2]) == 2);
}

void main() {
  testMinimumSwaps();
}