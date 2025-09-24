
int countPairsWithDifference(List<int> numbers, int difference)
/*
Given a sequence of positive integers and a positive integer $C$, the task is to calculate the number of pairs that satisfy the equation $A - B = C$ (pairs with identical numbers at different positions are considered distinct).

## Input Format

numbers: a list of integers $A_1, A_2, \ldots, A_N$
difference: an integer $C$ 

## Output Format

A single line indicating the number of pairs within the given sequence of positive integers that satisfy $A - B = C$.

## Sample #1
countPairsWithDifference([1, 1, 2, 3], 1) => 3
*/
{
  int count = 0;
  int n = numbers.length;
  for (int i = 0; i < n; i++) {
    for (int j = i + 1; j < n; j++) {
      if (numbers[i] - numbers[j] == difference) {
        count++;
      }
    }
  }
  return count;
}
void main() {
  testCountPairsWithDifference();
}

void testCountPairsWithDifference() {
  assert(countPairsWithDifference([1, 1, 2, 3], 1) == 3);
  assert(countPairsWithDifference([5, 4, 3, 2, 1], 1) == 0);
  assert(countPairsWithDifference([10, 2, 9, 4, 8, 3, 7], 5) == 1);
}