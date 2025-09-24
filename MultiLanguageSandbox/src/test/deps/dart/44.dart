
import 'dart:math';

int minimumTransfers(int n, List<int> groups, int L, int R)

/*
There are $n$ groups of students, with the initial number of students in each group provided. Additionally, the upper bound $R$ and the lower bound $L$ for the number of students per group are given, with the constraint that $L \le R$. At each step, you may select a student from one group and reassign them to another group. The question is: what is the minimum number of steps required to ensure that the number of students in each of the $N$ groups falls within the interval $[L,R]$?

## Input Format

n: the number of groups of students
groups: an array of $n$ integers, where the $i$-th integer represents the initial number of students in the $i$-th group
L: the lower bound for the number of students per group
R: the upper bound for the number of students per group

## Output Format

A single number, representing the minimum number of exchanges required. If the conditions of the problem cannot be met, output $-1$.

## Sample Case #1
minimumTransfers(2, [10, 20], 10, 15) => 5
 */
{
  int totalStudents = groups.reduce((a, b) => a + b);
  int minTotal = n * L;
  int maxTotal = n * R;
  
  if (totalStudents < minTotal || totalStudents > maxTotal) {
    return -1;
  }
  
  int transfers = 0;
  int excess = 0;
  int deficit = 0;
  
  for (int num in groups) {
    if (num < L) {
      deficit += (L - num);
    } else if (num > R) {
      excess += (num - R);
    }
  }
  
  // The transfers needed is the sum of deficits (or excesses, which should be equal)
  // But we need to ensure that the total excess covers the total deficit.
  // However, in the problem's constraints, it's guaranteed that totalStudents is between n*L and n*R,
  // so excess should equal deficit.
  if (excess != deficit) {
    return -1; // This case shouldn't happen if totalStudents is within bounds
  }
  
  return deficit; // or excess, since they are equal
}
void testMinimumTransfers() {
  assert(minimumTransfers(2, [10, 20], 10, 15) == 0);
  assert(minimumTransfers(3, [8, 16, 8], 9, 12) == 2);
  assert(minimumTransfers(4, [10, 10, 10, 10], 5, 15) == 0);
}

void main() {
  testMinimumTransfers();
}