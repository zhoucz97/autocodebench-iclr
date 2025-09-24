
int smallestNForSumExceeds(int k)
/*
Given that: $S_n= 1+\dfrac{1}{2}+\dfrac{1}{3}+…+\dfrac{1}{n}$. It is evident that for any integer $k$, when $n$ is sufficiently large, $S_n>k$.
Now, given an integer $k$, the task is to calculate the smallest $n$ such that $S_n>k$.
example
smallestNForSumExceeds(1) => 2
*/
{
  if (k < 1) {
    return 0; // though the problem likely expects k >=1 per the example
  }
  double sum = 0.0;
  int n = 0;
  while (sum <= k) {
    n++;
    sum += 1.0 / n;
  }
  return n;
}
void main() {
  check(smallestNForSumExceeds);
}

void check(int Function(int) smallestNForSumExceeds) {
  assert(smallestNForSumExceeds(1) == 2);
  assert(smallestNForSumExceeds(2) == 4);
  assert(smallestNForSumExceeds(0) == 1);
}