
import 'dart:math';

int maxSubarraySum(List<int> a)
/*
Given a sequence of length $n$, denoted as $a$, find a continuous and non-empty segment that yields the maximum sum.

## Input Format
a: a list of integers, $a_1, a_2, \ldots, a_n$

## Output Format

Output a single line with one integer representing the solution.

## Sample Case #1
maxSubarraySum([2, -4, 3, -1, 2, -4, 3]) => 4
*/
{
  if (a.isEmpty) return 0; // though problem states non-empty, handling edge case
  
  int maxCurrent = a[0];
  int maxGlobal = a[0];
  
  for (int i = 1; i < a.length; i++) {
    maxCurrent = (a[i] > maxCurrent + a[i]) ? a[i] : maxCurrent + a[i];
    if (maxCurrent > maxGlobal) {
      maxGlobal = maxCurrent;
    }
  }
  
  return maxGlobal;
}
void testMaxSubarraySum() {
  assert(maxSubarraySum([2, -4, 3, -1, 2, -4, 3]) == 4);
  assert(maxSubarraySum([-2, -3, 4, -1, -2, 1, 5, -3]) == 7);
  assert(maxSubarraySum([-1, -2, -3, -4]) == -1);
}

void main() {
  testMaxSubarraySum();
}