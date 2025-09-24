
int countSequences(int n)
/*Problem Description
YY contemplates the following scenario: a sequence of operation numbers, $1,2,\ldots ,n$ (as illustrated for the case of 1 to 3), where the depth of stack A exceeds $n$.

Currently, two operations can be performed:

1. Move a number from the head of the operation number sequence to the head of the stack (corresponding to the stack's push operation).
2. Move a number from the head of the stack to the tail of the output sequence (corresponding to the stack's pop operation).

Using these two operations, a series of output sequences can be generated from an operation number sequence, as shown below for generating the sequence `2 3 1` from `1 2 3`.

Your program is tasked with calculating and outputting the total number of possible output sequences derived from the operation number sequence $1,2,\ldots,n$.

example
countSequences(3) = 5
*/
{
  if (n == 0) return 1;
  int catalan = 1;
  for (int i = 1; i <= n; i++) {
    catalan *= (2 * n - i + 1);
    catalan ~/= i;
  }
  return catalan;
}
void testCountSequences() {
  // Test case 1: n = 3
  assert(countSequences(3) == 5, 'Test case 1 failed');

  // Test case 2: n = 4
  assert(countSequences(4) == 14, 'Test case 2 failed');

  // Test case 3: n = 5
  assert(countSequences(5) == 42, 'Test case 3 failed');

  print('All test cases passed!');
}

void main() {
  testCountSequences();
}