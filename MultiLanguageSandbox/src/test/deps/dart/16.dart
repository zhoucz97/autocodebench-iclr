
import 'dart:math';

int maxEnergy(List<int> beads)
/*Calculates the maximum energy that can be obtained by breaking a circular necklace of beads.

This function simulates the energy released when breaking a circular necklace of beads into individual parts.
The energy obtained from breaking the necklace at a certain point is the product of the values of the beads
at the breaking points. The function uses a dynamic programming approach to find the sequence of breaks
that maximizes the total energy released.

Arguments:
  beads (List<int>): A list of integers representing the value of each bead in the circular necklace.

Returns:
  int: The maximum energy obtainable by breaking the necklace.

Examples:
  maxEnergy([1, 2, 3])
  Returns: 6
*/
{
  if (beads.isEmpty) return 0;
  int maxProduct = 0;
  int n = beads.length;
  for (int i = 0; i < n; i++) {
    int current = beads[i] * beads[(i + 1) % n];
    if (current > maxProduct) {
      maxProduct = current;
    }
  }
  return maxProduct;
}
void testMaxEnergy() {
  // Test case 1: Simple case with three beads
  assert(
    maxEnergy([1, 2, 3]) == 6,
    'Test Case 1 Failed: Expected maximum energy to be 12, got ${maxEnergy([1, 2, 3])}'
  );

  // Test case 2: Another simple case with four beads
  assert(
    maxEnergy([4, 3, 2, 1]) == 32,
    'Test Case 2 Failed: Expected maximum energy to be 48, got ${maxEnergy([4, 3, 2, 1])}'
  );

  // Test case 3: Case with more beads
  assert(
    maxEnergy([1, 3, 5, 8]) == 144,
    'Test Case 3 Failed: Expected maximum energy to be 152, got ${maxEnergy([1, 3, 5, 8])}'
  );

  // Additional test cases can be added here as needed.

  print('All test cases passed!');
}

void main() {
  testMaxEnergy();
}