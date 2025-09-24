
int hanoiTwinTowers(int n)
/*
Given three sufficiently long thin rods named A, B, and C, there are $2n$ circular discs with holes in the middle placed on rod A. There are $n$ distinct sizes, with two identical discs for each size; note that these two discs are indistinguishable from each other

The task is to transfer these discs to rod C, with the possibility of temporarily storing them on rod B during the process. The requirements are as follows:

1. Only one disc can be moved at a time;
2. The discs on rods A, B, and C must always maintain an order with smaller discs on top of larger ones.

Task: Let $A_n$ be the minimum number of moves required to complete the above task for $2n$ discs. For the given input $n$, output $A_n$.

## Input Format

A positive integer $n$, indicating there are $2n$ circular discs placed on rod A.

## Output Format

A positive integer, which is the minimum number of moves required to complete the task $A_n$.

## Sample #1
hanoiTwinTowers(1) => 2
*/
{
  return (1 << (n + 1)) - 2;
}
void testHanoiTwinTowers() {
  assert(hanoiTwinTowers(1) == 2);
  assert(hanoiTwinTowers(2) == 8);
  assert(hanoiTwinTowers(3) == 26);
}

void main() {
  testHanoiTwinTowers();
}